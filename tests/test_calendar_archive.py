#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run calendar-archive against feeds served from a local HTTP server."""

from __future__ import annotations

import json
import os
import subprocess
import threading
from collections.abc import Iterator
from datetime import date, timedelta
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "calendar-archive"
TODAY = date.today()
FEEDS: dict[str, bytes] = {}


def ics(*events: str) -> bytes:
    body = "".join(events)
    return f"BEGIN:VCALENDAR\r\nVERSION:2.0\r\n{body}END:VCALENDAR\r\n".encode()


def event(uid: str, title: str, day: date, extra: str = "") -> str:
    d = day.strftime("%Y%m%d")
    return (
        f"BEGIN:VEVENT\r\nUID:{uid}\r\nSUMMARY:{title}\r\n"
        f"DTSTART;TZID=Australia/Sydney:{d}T100000\r\n"
        f"DTEND;TZID=Australia/Sydney:{d}T110000\r\n"
        f"X-MICROSOFT-CDO-BUSYSTATUS:BUSY\r\n{extra}END:VEVENT\r\n"
    )


class Handler(BaseHTTPRequestHandler):
    def do_GET(self) -> None:
        body = FEEDS.get(self.path.lstrip("/"))
        self.send_response(200 if body is not None else 404)
        self.end_headers()
        self.wfile.write(body or b"")

    def log_message(self, *args: object) -> None:
        pass


@pytest.fixture
def server() -> Iterator[str]:
    httpd = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=httpd.serve_forever, daemon=True).start()
    yield f"http://127.0.0.1:{httpd.server_port}"
    httpd.shutdown()


@pytest.fixture(autouse=True)
def feeds() -> Iterator[None]:
    quiet = ics(event("q", "Quiet", TODAY))
    FEEDS.update({"anu": quiet, "phdconvenor": quiet, "adirh": quiet})
    yield
    FEEDS.clear()


def run(server: str, out: Path) -> subprocess.CompletedProcess[str]:
    env = {
        **os.environ,
        "ANU_BEN_SWIFT_ICS_URL": f"{server}/anu",
        "PHDCONVENOR_ICS_URL": f"{server}/phdconvenor",
        "ADIRH_ICS_URL": f"{server}/adirh",
    }
    return subprocess.run(
        ["uv", "run", "--quiet", "--script", str(SCRIPT), "--out", str(out)],
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def titles(out: Path, name: str = "anu") -> list[str]:
    lines = (out / f"{name}.jsonl").read_text().splitlines()
    return [json.loads(line)["title"] for line in lines]


def test_recurrences_are_expanded_one_line_each(server: str, tmp_path: Path) -> None:
    FEEDS["anu"] = ics(
        event("a", "One-off", TODAY + timedelta(days=3)),
        event(
            "w", "Weekly", TODAY - timedelta(days=14), "RRULE:FREQ=WEEKLY;COUNT=4\r\n"
        ),
    )
    result = run(server, tmp_path)
    assert result.returncode == 0, result.stderr
    assert titles(tmp_path) == ["Weekly", "Weekly", "Weekly", "One-off", "Weekly"]
    first = json.loads((tmp_path / "anu.jsonl").read_text().splitlines()[0])
    assert first["start"].endswith(("+10:00", "+11:00"))
    assert first["busy"] == "busy" and first["all_day"] is False


def test_old_history_is_kept_and_recent_cancellations_dropped(
    server: str, tmp_path: Path
) -> None:
    FEEDS["anu"] = ics(
        event("old", "Aged out", TODAY - timedelta(days=400)),
        event("gone", "Cancelled later", TODAY - timedelta(days=20)),
        event("stay", "Still on", TODAY + timedelta(days=5)),
    )
    assert run(server, tmp_path).returncode == 0
    assert titles(tmp_path) == ["Aged out", "Cancelled later", "Still on"]

    # A year on, the feed has dropped the old event and the cancelled one.
    FEEDS["anu"] = ics(event("stay", "Still on", TODAY + timedelta(days=5)))
    result = run(server, tmp_path)
    assert result.returncode == 0, result.stderr
    assert titles(tmp_path) == ["Aged out", "Still on"]
    assert "anu: 2 occurrences (+0 -1)" in result.stdout


def test_an_empty_feed_fails_without_touching_the_archive(
    server: str, tmp_path: Path
) -> None:
    FEEDS["anu"] = ics(event("stay", "Still on", TODAY + timedelta(days=5)))
    assert run(server, tmp_path).returncode == 0
    before = (tmp_path / "anu.jsonl").read_text()

    FEEDS["anu"] = ics()
    del FEEDS["adirh"]
    result = run(server, tmp_path)
    assert result.returncode == 1
    assert (tmp_path / "anu.jsonl").read_text() == before
    assert "anu: the feed has no events" in result.stderr
    # The URL is the secret, so a failure names the variable instead.
    assert "$ADIRH_ICS_URL" in result.stderr and server not in result.stderr
    # The other calendars still ran.
    assert titles(tmp_path, "phdconvenor") == ["Quiet"]


if __name__ == "__main__":
    import sys

    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
