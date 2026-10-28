#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run calendar-archive against feeds served from a local HTTP server."""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import threading
from collections.abc import Iterator
from datetime import date, timedelta
from email.message import EmailMessage
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


def invite(
    path: Path, uid: str, day: date, sequence: int, body: str, extra: str = ""
) -> Path:
    """A meeting invite email, as Outlook sends one."""
    d = day.strftime("%Y%m%d")
    cal = (
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nMETHOD:REQUEST\r\nBEGIN:VEVENT\r\n"
        f"UID:{uid}\r\nSEQUENCE:{sequence}\r\nDTSTAMP:20260101T00000{sequence}Z\r\n"
        f"DTSTART;TZID=Australia/Sydney:{d}T100000\r\n"
        f"DTEND;TZID=Australia/Sydney:{d}T110000\r\nSUMMARY:Invite title\r\n"
        f"DESCRIPTION:{body}\r\n"
        "ORGANIZER;CN=Ada Lovelace:mailto:Ada@example.org\r\n"
        "ATTENDEE;CN=Ben Swift;PARTSTAT=NEEDS-ACTION:mailto:ben@example.org\r\n"
        f"{extra}END:VEVENT\r\nEND:VCALENDAR\r\n"
    )
    msg = EmailMessage()
    msg["Subject"] = "Invite"
    msg.set_content("You're invited")
    msg.add_attachment(
        cal.encode(), maintype="text", subtype="calendar", params={"method": "REQUEST"}
    )
    path.write_bytes(msg.as_bytes())
    return path


def run(
    server: str, out: Path, mail: list[Path] | None = None, mu: bool = True
) -> subprocess.CompletedProcess[str]:
    """Run the script with a fake mu that finds `mail`, or with no mu at all."""
    bin_dir = out / "bin"
    bin_dir.mkdir(exist_ok=True)
    listing = out / "mu-finds"
    listing.write_text("".join(f"{m}\n" for m in mail or []))
    fake = bin_dir / "mu"
    if mu:
        fake.write_text(f'#!/bin/sh\ncat "{listing}"\n[ -s "{listing}" ] || exit 4\n')
        fake.chmod(0o755)
    else:
        fake.unlink(missing_ok=True)
    env = {
        **os.environ,
        "PATH": f"{bin_dir}:/usr/bin:/bin",
        "ANU_BEN_SWIFT_ICS_URL": f"{server}/anu",
        "PHDCONVENOR_ICS_URL": f"{server}/phdconvenor",
        "ADIRH_ICS_URL": f"{server}/adirh",
    }
    uv = shutil.which("uv")
    assert uv
    return subprocess.run(
        [uv, "run", "--quiet", "--script", str(SCRIPT), "--out", str(out)],
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
    assert first["source"] == "feed"


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
    assert "anu: 2 occurrences, 0 with invite detail (+0 -1)" in result.stdout


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


def records(out: Path, name: str = "anu") -> list[dict[str, object]]:
    return [
        json.loads(line) for line in (out / f"{name}.jsonl").read_text().splitlines()
    ]


def test_events_take_detail_from_their_latest_invite(
    server: str, tmp_path: Path
) -> None:
    start = TODAY - timedelta(days=14)
    second = start + timedelta(days=7)
    FEEDS["anu"] = ics(
        event("w", "Weekly", start, "RRULE:FREQ=WEEKLY;COUNT=3\r\n"),
        event("a", "Made by Ben", TODAY + timedelta(days=3)),
    )
    rule = "_" * 40
    mail = [
        invite(tmp_path / "1.eml", "w", start, 0, "Old agenda"),
        invite(tmp_path / "2.eml", "w", start, 1, f"Agenda\\n{rule}\\nJoin on Teams"),
        invite(
            tmp_path / "3.eml",
            "w",
            second,
            2,
            "Moved room this week",
            f"RECURRENCE-ID;TZID=Australia/Sydney:{second:%Y%m%d}T100000\r\n",
        ),
    ]
    result = run(server, tmp_path, mail)
    assert result.returncode == 0, result.stderr
    weekly = [r for r in records(tmp_path) if r["uid"] == "w"]
    assert [r["description"] for r in weekly] == [
        "Agenda",
        "Moved room this week",
        "Agenda",
    ]
    # The feed, not the invite, decides the title.
    assert {r["title"] for r in weekly} == {"Weekly"}
    assert weekly[0]["organizer"] == {
        "email": "ada@example.org",
        "name": "Ada Lovelace",
    }
    assert weekly[0]["attendees"] == [{"email": "ben@example.org", "name": "Ben Swift"}]
    (own,) = [r for r in records(tmp_path) if r["uid"] == "a"]
    assert own["description"] is None and own["attendees"] == []
    assert "anu: 4 occurrences, 3 with invite detail" in result.stdout


def test_detail_survives_a_run_that_cannot_read_the_mail(
    server: str, tmp_path: Path
) -> None:
    FEEDS["anu"] = ics(event("m", "Meeting", TODAY + timedelta(days=2)))
    mail = [invite(tmp_path / "m.eml", "m", TODAY + timedelta(days=2), 0, "Agenda")]
    assert run(server, tmp_path, mail).returncode == 0

    result = run(server, tmp_path, mu=False)
    assert result.returncode == 1
    assert "calendar-archive:" in result.stderr
    (meeting,) = records(tmp_path)
    assert meeting["description"] == "Agenda"


if __name__ == "__main__":
    import sys

    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
