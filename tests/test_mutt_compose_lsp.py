#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pygls>=2.0.1"]
# ///

import importlib.machinery
import importlib.util
import subprocess
import sys
from pathlib import Path
from unittest.mock import patch

_lsp_path = str(Path(__file__).parent.parent / "bin" / "mutt-compose-lsp")
_loader = importlib.machinery.SourceFileLoader("mutt_compose_lsp", _lsp_path)
spec = importlib.util.spec_from_loader("mutt_compose_lsp", _loader, origin=_lsp_path)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

is_address_header = mod.is_address_header
extract_query = mod.extract_query
parse_contacts = mod.parse_contacts
format_display_name = mod.format_display_name
contacts_to_completion_items = mod.contacts_to_completion_items
run_mu_cfind = mod.run_mu_cfind
in_header_section = mod.in_header_section


class TestIsAddressHeader:
    def test_to(self):
        assert is_address_header("To: alice@example.com")

    def test_cc(self):
        assert is_address_header("Cc: bob@example.com")

    def test_bcc(self):
        assert is_address_header("Bcc: charlie@example.com")

    def test_from(self):
        assert is_address_header("From: dave@example.com")

    def test_reply_to(self):
        assert is_address_header("Reply-To: eve@example.com")

    def test_case_insensitive(self):
        assert is_address_header("TO: upper@example.com")
        assert is_address_header("cc: lower@example.com")
        assert is_address_header("bCC: mixed@example.com")

    def test_extra_space_before_colon(self):
        assert is_address_header("To : spaced@example.com")

    def test_subject_returns_false(self):
        assert not is_address_header("Subject: Hello")

    def test_date_returns_false(self):
        assert not is_address_header("Date: 2024-01-01")

    def test_body_text_returns_false(self):
        assert not is_address_header("Hello, this is the body")

    def test_empty_line_returns_false(self):
        assert not is_address_header("")

    def test_indented_returns_false(self):
        assert not is_address_header("  To: indented@example.com")


class TestExtractQuery:
    def test_basic_query(self):
        assert extract_query("To: ben", 7) == "ben"

    def test_empty_query(self):
        assert extract_query("To: ", 4) == ""

    def test_after_comma(self):
        assert extract_query("To: alice@example.com, bo", 25) == "bo"

    def test_before_colon_returns_none(self):
        assert extract_query("To: ben", 1) is None

    def test_at_colon_returns_none(self):
        assert extract_query("To: ben", 2) is None

    def test_multiple_commas(self):
        line = "To: alice@example.com, bob@example.com, ch"
        assert extract_query(line, len(line)) == "ch"

    def test_no_colon_returns_none(self):
        assert extract_query("no colon here", 5) is None

    def test_whitespace_after_comma_stripped(self):
        assert extract_query("To: alice@example.com,  ben", 27) == "ben"


class TestParseContacts:
    def test_valid_json(self):
        result = parse_contacts('[{"email": "a@b.com", "name": "A"}]')
        assert len(result) == 1
        assert result[0]["email"] == "a@b.com"

    def test_empty_array(self):
        assert parse_contacts("[]") == []

    def test_invalid_json(self):
        assert parse_contacts("not json") == []

    def test_none_input(self):
        assert parse_contacts(None) == []

    def test_non_array_json(self):
        assert parse_contacts('{"email": "a@b.com"}') == []


class TestFormatDisplayName:
    def test_name_and_email(self):
        assert format_display_name("Alice", "alice@example.com") == "Alice <alice@example.com>"

    def test_no_name(self):
        assert format_display_name(None, "alice@example.com") == "<alice@example.com>"

    def test_empty_name(self):
        assert format_display_name("", "alice@example.com") == "<alice@example.com>"

    def test_name_equals_email(self):
        assert format_display_name("a@b.com", "a@b.com") == "<a@b.com>"

    def test_quoted_name(self):
        assert format_display_name('"Bob Smith"', "bob@example.com") == '"Bob Smith" <bob@example.com>'


class TestContactsToCompletionItems:
    def test_single_contact(self):
        contacts = [{"email": "a@b.com", "name": "Alice", "frequency": 100}]
        items = contacts_to_completion_items(contacts)
        assert len(items) == 1
        assert items[0].label == "Alice <a@b.com>"
        assert items[0].insert_text == "Alice <a@b.com>"

    def test_frequency_sort_ordering(self):
        contacts = [
            {"email": "low@b.com", "name": "Low", "frequency": 10},
            {"email": "high@b.com", "name": "High", "frequency": 1000},
        ]
        items = contacts_to_completion_items(contacts)
        assert items[0].sort_text > items[1].sort_text  # lower freq = higher sort_text

    def test_no_name_fallback(self):
        contacts = [{"email": "a@b.com", "frequency": 5}]
        items = contacts_to_completion_items(contacts)
        assert items[0].label == "<a@b.com>"

    def test_empty_list(self):
        assert contacts_to_completion_items([]) == []

    def test_filter_text_includes_name_and_email(self):
        contacts = [{"email": "a@b.com", "name": "Alice", "frequency": 1}]
        items = contacts_to_completion_items(contacts)
        assert "Alice" in items[0].filter_text
        assert "a@b.com" in items[0].filter_text


class TestRunMuCfind:
    def test_success(self):
        mock_result = subprocess.CompletedProcess(
            args=[], returncode=0, stdout='[{"email": "a@b.com"}]'
        )
        with patch("subprocess.run", return_value=mock_result):
            assert run_mu_cfind("alice") == '[{"email": "a@b.com"}]'

    def test_timeout(self):
        with patch("subprocess.run", side_effect=subprocess.TimeoutExpired("mu", 5)):
            assert run_mu_cfind("alice") == "[]"

    def test_not_found(self):
        with patch("subprocess.run", side_effect=FileNotFoundError):
            assert run_mu_cfind("alice") == "[]"

    def test_nonzero_return_code(self):
        mock_result = subprocess.CompletedProcess(args=[], returncode=1, stdout="error")
        with patch("subprocess.run", return_value=mock_result):
            assert run_mu_cfind("alice") == "[]"

    def test_empty_query_returns_empty(self):
        assert run_mu_cfind("") == "[]"


class TestInHeaderSection:
    def test_first_line_is_header(self):
        assert in_header_section(["To: a@b.com", "Cc: c@d.com", "", "body"], 0)

    def test_second_line_is_header(self):
        assert in_header_section(["To: a@b.com", "Cc: c@d.com", "", "body"], 1)

    def test_after_blank_line_is_body(self):
        assert not in_header_section(["To: a@b.com", "", "body text"], 2)

    def test_single_header_line(self):
        assert in_header_section(["To: a@b.com"], 0)


if __name__ == "__main__":
    import pytest

    sys.exit(pytest.main([__file__, "-v"]))
