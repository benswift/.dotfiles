"""Tests for mutt-compose LSP functionality."""

import pytest

from mail_utils.lsp import (
    extract_query,
    find_body_start,
    format_display_name,
    format_file_size,
    get_greeting_name,
    greeting_completions,
    in_body_section,
    in_header_section,
    is_address_header,
    is_attach_header,
    parse_contacts,
    parse_to_header,
)


class TestHeaderDetection:
    @pytest.mark.parametrize(
        "line,expected",
        [
            ("To: someone@example.com", True),
            ("to: someone@example.com", True),
            ("Cc: someone@example.com", True),
            ("Bcc: someone@example.com", True),
            ("From: someone@example.com", True),
            ("Reply-To: someone@example.com", True),
            ("Subject: Hello", False),
            ("Attach: /path/to/file", False),
            ("Random text", False),
        ],
    )
    def test_is_address_header(self, line: str, expected: bool):
        assert is_address_header(line) == expected

    @pytest.mark.parametrize(
        "line,expected",
        [
            ("Attach: /path/to/file", True),
            ("attach: ~/file.pdf", True),
            ("To: someone@example.com", False),
            ("Subject: Hello", False),
        ],
    )
    def test_is_attach_header(self, line: str, expected: bool):
        assert is_attach_header(line) == expected


class TestExtractQuery:
    def test_extracts_after_colon(self):
        assert extract_query("To: john", 8) == "john"

    def test_extracts_last_segment_after_comma(self):
        assert extract_query("To: alice@example.com, bob", 26) == "bob"

    def test_returns_none_before_colon(self):
        assert extract_query("To: john", 2) is None

    def test_returns_empty_at_colon(self):
        assert extract_query("To: john", 3) == ""

    def test_handles_no_colon(self):
        assert extract_query("no colon here", 5) is None


class TestParseContacts:
    def test_parses_mutt_ab_format(self):
        output = "alice@example.com\tAlice Smith\nbob@example.com\tBob Jones\n"
        contacts = parse_contacts(output)
        assert len(contacts) == 2
        assert contacts[0]["email"] == "alice@example.com"
        assert contacts[0]["name"] == "Alice Smith"

    def test_handles_nil_name(self):
        output = "alice@example.com\tnil\n"
        contacts = parse_contacts(output)
        assert len(contacts) == 1
        assert contacts[0]["name"] == ""

    def test_handles_empty_output(self):
        assert parse_contacts("") == []

    def test_handles_malformed_lines(self):
        output = "no-tab-here\nalice@example.com\tAlice\n"
        contacts = parse_contacts(output)
        assert len(contacts) == 1


class TestFormatDisplayName:
    def test_formats_with_name(self):
        assert (
            format_display_name("Alice", "alice@example.com")
            == "Alice <alice@example.com>"
        )

    def test_formats_without_name(self):
        assert format_display_name(None, "alice@example.com") == "<alice@example.com>"

    def test_formats_when_name_equals_email(self):
        assert (
            format_display_name("alice@example.com", "alice@example.com")
            == "<alice@example.com>"
        )


class TestFormatFileSize:
    def test_bytes(self):
        assert format_file_size(500) == "500 B"

    def test_kilobytes(self):
        assert format_file_size(2048) == "2.0 KB"

    def test_megabytes(self):
        assert format_file_size(2 * 1024 * 1024) == "2.0 MB"


class TestSectionDetection:
    @pytest.fixture
    def email_lines(self) -> list[str]:
        return [
            "From: me@example.com",
            "To: you@example.com",
            "Subject: Hello",
            "",
            "Body text here",
            "More body",
        ]

    def test_find_body_start(self, email_lines: list[str]):
        assert find_body_start(email_lines) == 3

    def test_find_body_start_no_blank(self):
        lines = ["From: me@example.com", "To: you@example.com"]
        assert find_body_start(lines) is None

    def test_in_header_section(self, email_lines: list[str]):
        assert in_header_section(email_lines, 0) is True
        assert in_header_section(email_lines, 2) is True
        assert in_header_section(email_lines, 4) is False

    def test_in_body_section(self, email_lines: list[str]):
        assert in_body_section(email_lines, 0) is False
        assert in_body_section(email_lines, 3) is False
        assert in_body_section(email_lines, 4) is True


class TestParseToHeader:
    def test_parses_simple_address(self):
        lines = ["To: alice@example.com", ""]
        name, email = parse_to_header(lines)
        assert email == "alice@example.com"
        assert name is None

    def test_parses_name_and_address(self):
        lines = ["To: Alice Smith <alice@example.com>", ""]
        name, email = parse_to_header(lines)
        assert email == "alice@example.com"
        assert name == "Alice Smith"

    def test_parses_quoted_name(self):
        lines = ['To: "Alice Smith" <alice@example.com>', ""]
        name, email = parse_to_header(lines)
        assert email == "alice@example.com"
        assert name == "Alice Smith"

    def test_returns_none_when_missing(self):
        lines = ["From: me@example.com", ""]
        name, email = parse_to_header(lines)
        assert name is None
        assert email is None


class TestGetGreetingName:
    def test_extracts_first_name(self):
        lines = ["To: Alice Smith <alice@example.com>", ""]
        assert get_greeting_name(lines) == "Alice"

    def test_returns_none_without_to_header(self):
        lines = ["From: me@example.com", ""]
        assert get_greeting_name(lines) is None


class TestGreetingCompletions:
    @pytest.fixture
    def email_lines(self) -> list[str]:
        return ["To: Alice Smith <alice@example.com>", ""]

    def test_generates_completions_for_hey(self, email_lines: list[str]):
        items = greeting_completions(email_lines, "hey")
        assert len(items) == 1
        assert items[0].label == "Hey Alice,"
        assert items[0].insert_text == "Hey Alice,\n\n"

    def test_generates_completions_for_partial(self, email_lines: list[str]):
        items = greeting_completions(email_lines, "he")
        labels = [item.label for item in items]
        assert "Hey Alice," in labels
        assert "Hello Alice," in labels

    def test_returns_empty_without_recipient(self):
        lines = ["From: me@example.com", ""]
        items = greeting_completions(lines, "hey")
        assert items == []
