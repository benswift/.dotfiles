"""Tests for maildir operations."""

from pathlib import Path

import pytest

from mail_utils.maildir import (
    extract_flags,
    extract_hostname,
    extract_timestamp,
    extract_uid,
    generate_filename,
    get_all_message_files,
    is_maildir,
    is_mbsync_format,
    open_maildir,
)


class TestIsMaildir:
    def test_returns_true_for_valid_maildir(self, temp_maildir: Path):
        assert is_maildir(temp_maildir) is True

    def test_returns_false_for_empty_directory(self, tmp_path: Path):
        empty_dir = tmp_path / "empty"
        empty_dir.mkdir()
        assert is_maildir(empty_dir) is False

    def test_returns_false_for_regular_directory(self, tmp_path: Path):
        regular_dir = tmp_path / "regular"
        regular_dir.mkdir()
        (regular_dir / "somefile.txt").write_text("content")
        assert is_maildir(regular_dir) is False


class TestOpenMaildir:
    def test_opens_existing_maildir(self, temp_maildir: Path):
        mbox = open_maildir(temp_maildir)
        assert mbox is not None

    def test_opens_populated_maildir(self, populated_maildir: Path):
        mbox = open_maildir(populated_maildir)
        assert len(list(mbox.keys())) == 3


class TestExtractFlags:
    def test_extracts_single_flag(self):
        assert extract_flags("1234567890.R123.host,U=1:2,S") == "S"

    def test_extracts_multiple_flags(self):
        assert extract_flags("1234567890.R123.host,U=1:2,FRS") == "FRS"

    def test_returns_empty_for_no_flags(self):
        assert extract_flags("1234567890.R123.host,U=1:2,") == ""

    def test_returns_empty_for_malformed_filename(self):
        assert extract_flags("malformed-filename") == ""


class TestExtractUid:
    def test_extracts_uid(self):
        assert extract_uid("1234567890.R123.host,U=42:2,S") == "42"

    def test_extracts_large_uid(self):
        assert extract_uid("1234567890.R123.host,U=123456:2,S") == "123456"

    def test_returns_none_when_missing(self):
        assert extract_uid("1234567890.R123.host:2,S") is None

    def test_returns_none_for_malformed(self):
        assert extract_uid("malformed") is None


class TestExtractTimestamp:
    def test_extracts_timestamp(self):
        assert extract_timestamp("1704067200.R123.host,U=1:2,S") == 1704067200

    def test_returns_none_for_invalid(self):
        assert extract_timestamp("invalid.R123.host,U=1:2,S") is None

    def test_returns_none_for_short_number(self):
        assert extract_timestamp("12345.R123.host,U=1:2,S") is None


class TestExtractHostname:
    def test_extracts_hostname(self):
        assert extract_hostname("1704067200.R123.myhost,U=1:2,S") == "myhost"

    def test_handles_complex_hostname(self):
        assert extract_hostname("1704067200.R123.my-host-name,U=1:2,S") == "my-host-name"

    def test_returns_none_when_missing(self):
        assert extract_hostname("1704067200.R123:2,S") is None


class TestIsMbsyncFormat:
    def test_returns_true_for_valid_mbsync_format(self):
        assert is_mbsync_format("1704067200.R1234567890123456.host,U=1:2,S") is True

    def test_returns_false_for_old_format(self):
        assert is_mbsync_format("1704067200.12345_67890.host,U=1:2,S") is False

    def test_returns_false_for_invalid(self):
        assert is_mbsync_format("invalid-filename") is False


class TestGenerateFilename:
    def test_generates_mbsync_format(self):
        filename = generate_filename(1704067200, "testhost", "42", "S")
        assert filename.startswith("1704067200.R")
        assert ".testhost,U=42:2,S" in filename

    def test_generates_without_uid(self):
        filename = generate_filename(1704067200, "testhost", None, "RS")
        assert filename.startswith("1704067200.R")
        assert "testhost:2,RS" in filename
        assert ",U=" not in filename

    def test_generates_empty_flags(self):
        filename = generate_filename(1704067200, "testhost", "1", "")
        assert filename.endswith(":2,")

    def test_sequential_format(self):
        filename = generate_filename(1704067200, "testhost", "1", "S", use_random_id=False)
        assert filename.startswith("1704067200.")
        assert ".R" not in filename


class TestGetAllMessageFiles:
    def test_returns_empty_for_empty_maildir(self, temp_maildir: Path):
        files = get_all_message_files(temp_maildir)
        assert files == []

    def test_returns_files_from_populated_maildir(self, populated_maildir: Path):
        files = get_all_message_files(populated_maildir)
        assert len(files) == 3

    def test_returns_tuples_of_dir_and_path(self, populated_maildir: Path):
        files = get_all_message_files(populated_maildir)
        for directory, file_path in files:
            assert directory.is_dir()
            assert file_path.is_file()
