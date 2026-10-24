"""Serve one kiosk's install files and record its progress callbacks.

    serve.py <address> <port> <directory> <events-file>

GETs of files in <directory> are served as-is. Three paths are progress
signals rather than files, each appended to <events-file> as "<name> <ip>"
for bin/kiosk to wait on:

    preseed.cfg   (also served) the installer has booted and fetched its answers
    /installed    the installer's late_command ran; it is rebooting
    /phone-home   cloud-init's phone_home (POST): first boot configured the kiosk

Standard library only, so it runs under any python3 without a venv.
"""

from __future__ import annotations

import sys
from contextlib import suppress
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path


class Handler(SimpleHTTPRequestHandler):
    def __init__(self, *args, events: Path, **kwargs) -> None:
        self.events = events
        super().__init__(*args, **kwargs)

    def record(self, name: str) -> None:
        with self.events.open("a") as log:
            log.write(f"{name} {self.client_address[0]}\n")

    def do_GET(self) -> None:
        if self.path == "/installed":
            self.record("installed")
            self.send_response(204)
            self.end_headers()
            return
        if self.path == "/preseed.cfg":
            self.record("preseed")
        super().do_GET()

    def do_POST(self) -> None:
        if self.path != "/phone-home":
            self.send_error(404)
            return
        self.rfile.read(int(self.headers.get("Content-Length", 0)))
        self.record("phone-home")
        self.send_response(204)
        self.end_headers()

    def log_message(self, format: str, *args: object) -> None:
        pass


def main() -> None:
    address, port, directory, events = sys.argv[1:]
    handler = partial(Handler, directory=directory, events=Path(events))
    # Ctrl-C at bin/kiosk reaches this process too; teardown is bin/kiosk's job.
    with suppress(KeyboardInterrupt):
        ThreadingHTTPServer((address, int(port)), handler).serve_forever()


if __name__ == "__main__":
    main()
