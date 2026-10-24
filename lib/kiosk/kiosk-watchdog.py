#!/usr/bin/env python3
"""Restart the kiosk browser unless it shows a live page from the kiosk URL.

Chromium's own error pages (no network yet, an upstream 5xx) and a crashed
renderer ("Aw, Snap!") otherwise stay on screen until the nightly restart.
A page counts as live when it is on the kiosk URL's origin and its renderer
answers a script evaluation over the local DevTools port. A second check a
minute later keeps a page that is merely mid-load from triggering a restart.
"""

import base64
import json
import os
import socket
import struct
import subprocess
import time
import urllib.request
from pathlib import Path
from urllib.parse import urlsplit

DEVTOOLS = "http://127.0.0.1:9222"
URL_FILE = Path("/boot/firmware/kiosk-url.txt")
DEFAULT_URL_FILE = Path("/etc/kiosk-url.txt")
UNIT = "chromium-kiosk.service"


def kiosk_origin() -> str:
    url = (URL_FILE if URL_FILE.exists() else DEFAULT_URL_FILE).read_text().strip()
    parts = urlsplit(url)
    return f"{parts.scheme}://{parts.netloc}/"


def recv_exact(sock: socket.socket, n: int, buf: bytes) -> tuple[bytes, bytes]:
    while len(buf) < n:
        chunk = sock.recv(4096)
        if not chunk:
            raise ConnectionError("DevTools socket closed")
        buf += chunk
    return buf[:n], buf[n:]


def renderer_answers(ws_url: str) -> bool:
    """Evaluate 1 + 1 in the page over a minimal websocket client."""
    parts = urlsplit(ws_url)
    with socket.create_connection((parts.hostname, parts.port), timeout=10) as sock:
        key = base64.b64encode(os.urandom(16)).decode()
        sock.sendall(
            f"GET {parts.path} HTTP/1.1\r\nHost: {parts.netloc}\r\n"
            "Upgrade: websocket\r\nConnection: Upgrade\r\n"
            f"Sec-WebSocket-Key: {key}\r\nSec-WebSocket-Version: 13\r\n\r\n".encode()
        )
        buf = b""
        while b"\r\n\r\n" not in buf:
            chunk = sock.recv(4096)
            if not chunk:
                raise ConnectionError("DevTools socket closed")
            buf += chunk
        head, buf = buf.split(b"\r\n\r\n", 1)
        if b" 101 " not in head.split(b"\r\n", 1)[0]:
            return False

        msg = json.dumps(
            {
                "id": 1,
                "method": "Runtime.evaluate",
                "params": {"expression": "1 + 1", "returnByValue": True},
            }
        ).encode()
        mask = os.urandom(4)
        length = (
            bytes([0x80 | len(msg)])
            if len(msg) < 126
            else bytes([0x80 | 126]) + struct.pack(">H", len(msg))
        )
        masked = bytes(b ^ mask[i % 4] for i, b in enumerate(msg))
        sock.sendall(bytes([0x81]) + length + mask + masked)

        while True:
            header, buf = recv_exact(sock, 2, buf)
            n = header[1] & 0x7F
            if n == 126:
                ext, buf = recv_exact(sock, 2, buf)
                n = struct.unpack(">H", ext)[0]
            elif n == 127:
                ext, buf = recv_exact(sock, 8, buf)
                n = struct.unpack(">Q", ext)[0]
            payload, buf = recv_exact(sock, n, buf)
            if header[0] & 0x0F != 1:
                continue
            reply = json.loads(payload)
            if reply.get("id") == 1:
                return reply.get("result", {}).get("result", {}).get("value") == 2


def healthy() -> bool:
    origin = kiosk_origin()
    try:
        with urllib.request.urlopen(f"{DEVTOOLS}/json/list", timeout=5) as resp:
            targets = json.load(resp)
        return any(
            renderer_answers(t["webSocketDebuggerUrl"])
            for t in targets
            if t.get("type") == "page"
            and t.get("url", "").startswith(origin)
            and "webSocketDebuggerUrl" in t
        )
    except (OSError, ValueError):
        return False


def main() -> None:
    active = subprocess.run(
        ["systemctl", "--user", "is-active", "--quiet", UNIT], check=False
    )
    if active.returncode != 0 or healthy():
        return
    time.sleep(60)
    if healthy():
        return
    print("kiosk not showing a live page from the kiosk URL; restarting")
    subprocess.run(["systemctl", "--user", "restart", UNIT], check=True)


if __name__ == "__main__":
    main()
