#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist", "pyyaml"]
# ///
"""Run calendar-archive against feeds served from a local HTTP server, writing
event records into a temporary vault."""

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
import yaml

SCRIPT = Path(__file__).parent.parent / "bin" / "calendar-archive"
TODAY = date.today()
FEEDS: dict[str, bytes] = {}
CALENDARS = ("--calendar", "anu", "--calendar", "phdconvenor", "--calendar", "adirh")


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


@pytest.fixture
def vault(tmp_path: Path) -> Path:
    v = tmp_path / "vault"
    v.mkdir()
    return v


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
    server: str,
    vault: Path,
    *args: str,
    mail: list[Path] | None = None,
    readable: bool = True,
) -> subprocess.CompletedProcess[str]:
    """Run the script from vault, with fake mu and wiz: mu finds `mail` (or,
    unreadable, a message that isn't there), Ada is a person in the vault and
    Ben one in the lx vault."""
    bin_dir = vault.parent / "bin"
    bin_dir.mkdir(exist_ok=True)
    listing = vault.parent / "mu-finds"
    found = [*(mail or [])] if readable else [vault.parent / "no-such-message.eml"]
    listing.write_text("".join(f"{m}\n" for m in found))
    (bin_dir / "mu").write_text(
        f'#!/bin/sh\ncat "{listing}"\n[ -s "{listing}" ] || exit 4\n'
    )
    here = [{"path": "people/ada-lovelace.md", "fields": {"email": "ada@example.org"}}]
    lx = [{"path": "people/ben-swift.md", "fields": {"email": "ben@example.org"}}]
    (bin_dir / "wiz").write_text(
        "#!/bin/sh\n"
        'case "$1 $2" in\n'
        f"  'query lx') echo '{json.dumps(lx)}' ;;\n"
        f"  query*) echo '{json.dumps(here)}' ;;\n"
        '  validate*) echo \'[{"vault": "x", "problems": []}]\' ;;\n'
        "esac\n"
    )
    for fake in ("mu", "wiz"):
        (bin_dir / fake).chmod(0o755)
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
        [uv, "run", "--quiet", "--script", str(SCRIPT), *args],
        cwd=vault,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def texts(vault: Path, calendar: str = "anu") -> dict[str, str]:
    folder = vault / "events" / calendar
    return {
        str(p.relative_to(vault)): p.read_text() for p in sorted(folder.rglob("*.md"))
    }


def records(vault: Path, calendar: str = "anu") -> list[dict[str, object]]:
    """The calendar's records, in start order."""
    out = []
    for text in texts(vault, calendar).values():
        out.append(yaml.safe_load(text.removeprefix("---\n").split("\n---\n")[0]))
    return sorted(out, key=lambda r: (str(r["start_date"]), str(r["uid"])))


def titles(vault: Path, calendar: str = "anu") -> list[str]:
    return [str(r["title"]) for r in records(vault, calendar)]


def test_recurrences_are_expanded_one_record_each(server: str, vault: Path) -> None:
    FEEDS["anu"] = ics(
        event("a", "One-off", TODAY + timedelta(days=3)),
        event(
            "w", "Weekly", TODAY - timedelta(days=14), "RRULE:FREQ=WEEKLY;COUNT=4\r\n"
        ),
    )
    result = run(server, vault, *CALENDARS)
    assert result.returncode == 0, result.stderr
    assert titles(vault) == ["Weekly", "Weekly", "Weekly", "One-off", "Weekly"]
    first = records(vault)[0]
    assert str(first["start_date"]).endswith(("+10:00", "+11:00"))
    assert first["busy"] == "busy" and first["all_day"] is False
    assert first["source"] == "feed" and first["calendar"] == "anu"


def test_old_history_is_kept_and_recent_cancellations_dropped(
    server: str, vault: Path
) -> None:
    FEEDS["anu"] = ics(
        event("old", "Aged out", TODAY - timedelta(days=400)),
        event("gone", "Cancelled later", TODAY - timedelta(days=20)),
        event("stay", "Still on", TODAY + timedelta(days=5)),
    )
    assert run(server, vault, *CALENDARS).returncode == 0
    assert titles(vault) == ["Aged out", "Cancelled later", "Still on"]

    # A year on, the feed has dropped the old event and the cancelled one.
    FEEDS["anu"] = ics(event("stay", "Still on", TODAY + timedelta(days=5)))
    result = run(server, vault, *CALENDARS)
    assert result.returncode == 0, result.stderr
    assert titles(vault) == ["Aged out", "Still on"]
    assert "anu: 2 occurrences, 0 with invite detail (+0 -1)" in result.stdout


def test_an_empty_feed_fails_without_touching_the_archive(
    server: str, vault: Path
) -> None:
    FEEDS["anu"] = ics(event("stay", "Still on", TODAY + timedelta(days=5)))
    assert run(server, vault, *CALENDARS).returncode == 0
    before = texts(vault)

    FEEDS["anu"] = ics()
    del FEEDS["adirh"]
    result = run(server, vault, *CALENDARS)
    assert result.returncode == 1
    assert texts(vault) == before
    assert "anu: the feed has no events" in result.stderr
    # The URL is the secret, so a failure names the variable instead.
    assert "$ADIRH_ICS_URL" in result.stderr and server not in result.stderr
    # The other calendars still ran.
    assert titles(vault, "phdconvenor") == ["Quiet"]


def test_events_take_detail_from_their_latest_invite(
    server: str, vault: Path, tmp_path: Path
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
    result = run(server, vault, *CALENDARS, mail=mail)
    assert result.returncode == 0, result.stderr
    weekly = [r for r in records(vault) if r["uid"] == "w"]
    assert [r["description"] for r in weekly] == [
        "Agenda",
        "Moved room this week",
        "Agenda",
    ]
    # The feed, not the invite, decides the title.
    assert {r["title"] for r in weekly} == {"Weekly"}
    # A person in the vault is linked; anyone else is their name and address.
    assert weekly[0]["organizer"] == "[[people/ada-lovelace]]"
    assert weekly[0]["attendee"] == ["Ben Swift <ben@example.org>"]
    (own,) = [r for r in records(vault) if r["uid"] == "a"]
    assert "description" not in own and "attendee" not in own
    assert "anu: 4 occurrences, 3 with invite detail" in result.stdout


def test_detail_survives_a_run_that_cannot_read_the_mail(
    server: str, vault: Path, tmp_path: Path
) -> None:
    FEEDS["anu"] = ics(event("m", "Meeting", TODAY + timedelta(days=2)))
    mail = [invite(tmp_path / "m.eml", "m", TODAY + timedelta(days=2), 0, "Agenda")]
    assert run(server, vault, *CALENDARS, mail=mail).returncode == 0

    result = run(server, vault, *CALENDARS, readable=False)
    assert result.returncode == 1
    assert "calendar-archive:" in result.stderr
    (meeting,) = records(vault)
    assert meeting["description"] == "Agenda"


def test_text_with_unicode_line_separators_round_trips(
    server: str, vault: Path
) -> None:
    FEEDS["anu"] = ics(event("u", "Line separated", TODAY + timedelta(days=1)))
    assert run(server, vault, *CALENDARS).returncode == 0
    before = texts(vault)
    result = run(server, vault, *CALENDARS)
    assert result.returncode == 0, result.stderr
    assert titles(vault) == ["Line separated"]
    assert texts(vault) == before


def test_unchanged_runs_touch_nothing_and_people_link_across_vaults(
    server: str, vault: Path, tmp_path: Path
) -> None:
    day = TODAY + timedelta(days=2)
    FEEDS["anu"] = ics(
        event("m", "Planning: next year", day),
        event(
            "w", "Weekly", TODAY - timedelta(days=7), "RRULE:FREQ=WEEKLY;COUNT=2\r\n"
        ),
    )
    mail = [invite(tmp_path / "m.eml", "m", day, 0, "Agenda")]
    assert run(server, vault, "--calendar", "anu", mail=mail).returncode == 0
    paths = list(texts(vault))
    assert any(
        p.startswith(f"events/anu/{day:%Y}/{day:%Y-%m-%d}-planning-next-year-")
        for p in paths
    )
    stamps = {p: (vault / p).stat().st_mtime_ns for p in paths}
    assert run(server, vault, "--calendar", "anu", mail=mail).returncode == 0
    assert {p: (vault / p).stat().st_mtime_ns for p in paths} == stamps

    FEEDS["anu"] = ics(event("m", "Planning: next year", day))
    result = run(server, vault, "--calendar", "anu", "--link-vault", "lx", mail=mail)
    assert result.returncode == 0, result.stderr
    (meeting,) = records(vault)
    assert meeting["attendee"] == ["[[lx:people/ben-swift]]"]


def test_history_is_seeded_from_the_old_jsonl_archive(
    server: str, vault: Path, tmp_path: Path
) -> None:
    old = TODAY - timedelta(days=500)
    seed = tmp_path / "jsonl"
    seed.mkdir()
    (seed / "anu.jsonl").write_text(
        json.dumps(
            {
                "calendar": "anu",
                "uid": "old",
                "title": "Long ago",
                "location": None,
                "start": f"{old}T10:00:00+11:00",
                "end": f"{old}T11:00:00+11:00",
                "all_day": False,
                "busy": "busy",
                "source": "Outlook export",
                "description": None,
                "organizer": None,
                "attendees": [],
            }
        )
        + "\n"
    )
    FEEDS["anu"] = ics(event("now", "Now", TODAY))
    result = run(server, vault, "--calendar", "anu", "--seed", str(seed))
    assert result.returncode == 0, result.stderr
    assert titles(vault) == ["Long ago", "Now"]
    assert records(vault)[0]["source"] == "Outlook export"


if __name__ == "__main__":
    import sys

    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
