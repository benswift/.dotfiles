#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12,<3.14"
# dependencies = []
# ///
# nb-capture-server.py --- HTTP endpoint for iOS Shortcut audio uploads
#
# Companion to nb-capture-todo.py. Receives base64-encoded audio via
# JSON POST from the iOS Shortcut, decodes it, saves to a temp file,
# and runs nb-capture-todo.py in the background.
#
# Binds to 0.0.0.0:8765. Only expose via Tailscale (firewall or ACLs).
#
# Usage:
#   nb-capture-server.py
#
# Expected JSON body:
#   {"audio": "<base64>", "timestamp": "20260312-101500"}
#
# Query parameters:
#   title (required, URL-encoded)
#   url (optional, URL-encoded)

import base64
import json
import subprocess
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from urllib.parse import parse_qs, urlparse

SCRIPT_PATH = Path(__file__).parent / "nb-capture-todo.py"
PORT = 8765


class CaptureHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        parsed = urlparse(self.path)
        if parsed.path != "/capture":
            self.send_error(404)
            return

        params = parse_qs(parsed.query)
        title = params.get("title", [""])[0]
        url = params.get("url", [""])[0]

        content_length = int(self.headers.get("Content-Length", 0))
        body = json.loads(self.rfile.read(content_length))

        audio_b64 = body.get("audio", "")
        timestamp = body.get("timestamp", "unknown")
        timestamp = "".join(c for c in timestamp if c.isalnum() or c == "-")

        audio_path = Path(f"/tmp/nb-capture-{timestamp}.m4a")
        audio_path.write_bytes(base64.b64decode(audio_b64))

        cmd = [str(SCRIPT_PATH), "--title", title, "--audio", str(audio_path)]
        if url:
            cmd.extend(["--url", url])

        subprocess.Popen(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )

        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps({"status": "ok"}).encode())

    def log_message(self, format, *args):
        print(f"{self.client_address[0]} - {format % args}", file=sys.stderr)


def main():
    server = HTTPServer(("0.0.0.0", PORT), CaptureHandler)
    print(f"nb-capture-server listening on port {PORT}", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()
