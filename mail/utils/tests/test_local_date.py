"""Tests for the neomutt display filter that localises the Date: header."""

import subprocess
import sys
import time

import pytest
from mail_utils.cli.local_date import rewrite


@pytest.fixture(autouse=True)
def sydney(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv("TZ", "Australia/Sydney")
    time.tzset()
    yield
    monkeypatch.undo()
    time.tzset()


def message(date: str, body: str = "Hello.\n") -> str:
    return f"From: A <a@example.com>\nDate: {date}\nSubject: hi\n\n{body}"


class TestRewrite:
    def test_converts_sender_zone_to_local(self):
        out = rewrite(message("Mon, 14 Sep 2026 06:30:00 +0100"))
        assert "Date: Mon, 14 Sep 2026 15:30 AEST\n" in out

    def test_crosses_the_date_line(self):
        out = rewrite(message("Sun, 13 Sep 2026 20:00:00 -0700"))
        assert "Date: Mon, 14 Sep 2026 13:00 AEST\n" in out

    def test_unknown_origin_zone_is_utc(self):
        out = rewrite(message("Mon, 14 Sep 2026 00:00:00 -0000"))
        assert "Date: Mon, 14 Sep 2026 10:00 AEST\n" in out

    def test_daylight_saving(self):
        out = rewrite(message("Mon, 14 Dec 2026 00:00:00 +0000"))
        assert "Date: Mon, 14 Dec 2026 11:00 AEDT\n" in out

    def test_unparseable_date_passes_through(self):
        original = message("sometime last week")
        assert rewrite(original) == original

    def test_body_is_untouched(self):
        body = "> Date: Mon, 14 Sep 2026 06:30:00 +0100\n"
        out = rewrite(message("Mon, 14 Sep 2026 06:30:00 +0100", body))
        assert out.endswith(body)

    def test_crlf_endings_preserved(self):
        original = message("Mon, 14 Sep 2026 06:30:00 +0100").replace("\n", "\r\n")
        assert "Date: Mon, 14 Sep 2026 15:30 AEST\r\n" in rewrite(original)


def test_filter_round_trips_non_utf8_bytes():
    raw = message("Mon, 14 Sep 2026 06:30:00 +0100", "caf\xe9\n").encode("latin-1")
    result = subprocess.run(
        [sys.executable, "-m", "mail_utils.cli.local_date"],
        input=raw,
        capture_output=True,
        check=True,
    )
    assert b"Date: Mon, 14 Sep 2026 15:30 AEST\n" in result.stdout
    assert result.stdout.endswith(b"caf\xe9\n")
