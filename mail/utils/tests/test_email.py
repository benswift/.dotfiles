"""Tests for email parsing utilities."""

from email.message import EmailMessage


from mail_utils.email import (
    get_message_id,
    parse_email_date,
    read_email_from_bytes,
)


class TestGetMessageId:
    def test_extracts_message_id(self, sample_email: EmailMessage):
        result = get_message_id(sample_email)
        assert result == "test-message-id-12345@example.com"

    def test_strips_angle_brackets(self):
        msg = EmailMessage()
        msg["Message-ID"] = "<  bracketed-id@example.com  >"
        result = get_message_id(msg)
        assert result == "bracketed-id@example.com"

    def test_returns_none_when_missing(self):
        msg = EmailMessage()
        msg["From"] = "test@example.com"
        result = get_message_id(msg)
        assert result is None

    def test_handles_whitespace(self):
        msg = EmailMessage()
        msg["Message-ID"] = "  <whitespace-id@example.com>  "
        result = get_message_id(msg)
        assert result == "whitespace-id@example.com"


class TestParseEmailDate:
    def test_parses_standard_rfc2822_date(self):
        result = parse_email_date("Mon, 01 Jan 2024 12:00:00 +0000")
        assert result is not None
        assert result.year == 2024
        assert result.month == 1
        assert result.day == 1
        assert result.hour == 12

    def test_parses_date_with_timezone(self):
        result = parse_email_date("Tue, 02 Jan 2024 15:30:00 -0500")
        assert result is not None
        assert result.year == 2024

    def test_parses_nonstandard_date_with_dateutil(self):
        result = parse_email_date("January 1, 2024")
        assert result is not None
        assert result.year == 2024
        assert result.month == 1

    def test_returns_none_for_empty_string(self):
        result = parse_email_date("")
        assert result is None

    def test_returns_none_for_invalid_date(self):
        result = parse_email_date("not a date at all xyz")
        assert result is None


class TestReadEmailFromBytes:
    def test_parses_email_from_bytes(self, sample_email_bytes: bytes):
        msg = read_email_from_bytes(sample_email_bytes)
        assert msg["From"] == "sender@example.com"
        assert msg["To"] == "recipient@example.com"
        assert msg["Subject"] == "Test Subject"

    def test_preserves_message_id(self, sample_email_bytes: bytes):
        msg = read_email_from_bytes(sample_email_bytes)
        assert get_message_id(msg) == "test-message-id-12345@example.com"
