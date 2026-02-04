"""Tests for deduplication functionality."""

from pathlib import Path

import pytest

from mail_utils.cli.dedupe import deduplicate, find_duplicates
from mail_utils.maildir import open_maildir


class TestFindDuplicates:
    def test_finds_no_duplicates_in_unique_maildir(self, populated_maildir: Path):
        mbox = open_maildir(populated_maildir)
        message_id_to_keys, total = find_duplicates(mbox, show_progress=False)

        assert total == 3
        assert len(message_id_to_keys) == 3
        for keys in message_id_to_keys.values():
            assert len(keys) == 1

    def test_finds_duplicates(self, maildir_with_duplicates: Path):
        mbox = open_maildir(maildir_with_duplicates)
        message_id_to_keys, total = find_duplicates(mbox, show_progress=False)

        assert total == 4
        assert len(message_id_to_keys) == 2

        duplicate_keys = message_id_to_keys.get("<duplicate-id@example.com>", [])
        assert len(duplicate_keys) == 3

        unique_keys = message_id_to_keys.get("<unique-id@example.com>", [])
        assert len(unique_keys) == 1


class TestDeduplicate:
    def test_dry_run_does_not_delete(self, maildir_with_duplicates: Path):
        total, unique, removed = deduplicate(
            maildir_with_duplicates, dry_run=True, verbose=False
        )

        assert total == 4
        assert unique == 2
        assert removed == 2

        mbox = open_maildir(maildir_with_duplicates)
        assert len(list(mbox.keys())) == 4

    def test_removes_duplicates(self, maildir_with_duplicates: Path):
        total, unique, removed = deduplicate(
            maildir_with_duplicates, dry_run=False, verbose=False
        )

        assert removed == 2

        mbox = open_maildir(maildir_with_duplicates)
        remaining_keys = list(mbox.keys())
        assert len(remaining_keys) == 2

        remaining_ids = set()
        for key in remaining_keys:
            msg = mbox[key]
            remaining_ids.add(msg.get("Message-ID"))

        assert "<duplicate-id@example.com>" in remaining_ids
        assert "<unique-id@example.com>" in remaining_ids

    def test_handles_empty_maildir(self, temp_maildir: Path):
        total, unique, removed = deduplicate(temp_maildir, dry_run=False, verbose=False)
        assert total == 0
        assert unique == 0
        assert removed == 0
