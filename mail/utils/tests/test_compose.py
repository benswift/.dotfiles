"""Tests for email composition utilities."""

import json
from pathlib import Path
from unittest.mock import MagicMock, patch

from typer.testing import CliRunner

from mail_utils.accounts import Account, get_account_config
from mail_utils.cli.compose import app
from mail_utils.compose import (
    build_email,
    combine_cc,
    send_email,
    strip_frontmatter,
)


class TestBuildEmail:
    def test_builds_basic_email(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test Subject",
            body="Test body",
        )

        assert msg["From"] == "sender@example.com"
        assert msg["To"] == "recipient@example.com"
        assert msg["Subject"] == "Test Subject"
        assert msg["Date"] is not None
        assert msg["Message-ID"] is not None
        assert "Test body" in msg.as_string()

    def test_includes_cc_when_provided(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
            cc="cc@example.com",
        )

        assert msg["Cc"] == "cc@example.com"

    def test_no_cc_when_not_provided(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
        )

        assert msg["Cc"] is None

    def test_adds_attachment(self, tmp_path: Path):
        attachment = tmp_path / "test.txt"
        attachment.write_text("attachment content")

        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
            attachments=[attachment],
        )

        assert msg.is_multipart()

    def test_ignores_nonexistent_attachment(self, tmp_path: Path):
        nonexistent = tmp_path / "nonexistent.txt"

        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
            attachments=[nonexistent],
        )

        assert not msg.is_multipart()


class TestStripFrontmatter:
    def test_strips_yaml_frontmatter(self):
        content = """---
title: Test
---
Body content"""
        result = strip_frontmatter(content)
        assert result == "Body content"

    def test_preserves_content_without_frontmatter(self):
        content = "Just body content"
        result = strip_frontmatter(content)
        assert result == "Just body content"

    def test_handles_multiline_frontmatter(self):
        content = """---
title: Test
author: Someone
---
Body"""
        result = strip_frontmatter(content)
        assert result == "Body"


class TestCombineCc:
    def test_combines_both_cc(self):
        result = combine_cc("a@example.com", "b@example.com")
        assert result == "a@example.com, b@example.com"

    def test_returns_cc_when_no_cc_all(self):
        result = combine_cc("a@example.com", None)
        assert result == "a@example.com"

    def test_returns_cc_all_when_no_cc(self):
        result = combine_cc(None, "b@example.com")
        assert result == "b@example.com"

    def test_returns_none_when_both_empty(self):
        result = combine_cc(None, None)
        assert result is None


class TestSendEmail:
    def test_dry_run_returns_success_without_sending(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
        )

        success, message = send_email(msg, Account.personal, dry_run=True)

        assert success is True
        assert "Would send" in message

    def test_calls_msmtp_with_correct_args(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
        )

        with (
            patch("subprocess.run") as mock_run,
            patch("mail_utils.compose.save_to_sent"),
        ):
            mock_run.return_value = MagicMock(returncode=0)
            success, _ = send_email(msg, Account.personal, dry_run=False)

            assert success is True
            mock_run.assert_called_once()
            args = mock_run.call_args[0][0]
            assert args[0] == "msmtp"
            assert "-t" in args
            assert "-a" in args
            assert "personal" in args


class TestGetAccountConfig:
    def test_returns_config_for_personal(self):
        config = get_account_config(Account.personal)
        assert "benswift.me" in config.from_addr
        assert config.msmtp == "personal"

    def test_returns_config_for_anu(self):
        config = get_account_config(Account.anu)
        assert "anu.edu.au" in config.from_addr
        assert config.msmtp == "anu"

    def test_accepts_string_account(self):
        config = get_account_config("personal")
        assert config.msmtp == "personal"


class TestComposeCLI:
    runner = CliRunner()

    def test_batch_reads_data_from_stdin(self):
        data = [{"email": "test@example.com", "name": "Test User"}]
        result = self.runner.invoke(
            app,
            [
                "-f",
                "personal",
                "--data",
                "-",
                "--to",
                "{{email}}",
                "--subject",
                "Hello {{name}}",
                "--body",
                "Test body",
                "--dry-run",
            ],
            input=json.dumps(data),
        )

        assert result.exit_code == 0
        assert "test@example.com" in result.stdout
        assert "Hello Test User" in result.stdout

    def test_batch_reads_data_from_file(self, tmp_path: Path):
        data_file = tmp_path / "data.json"
        data_file.write_text('[{"email": "file@example.com", "name": "File User"}]')

        result = self.runner.invoke(
            app,
            [
                "-f",
                "personal",
                "--data",
                str(data_file),
                "--to",
                "{{email}}",
                "--subject",
                "Hello {{name}}",
                "--body",
                "Test body",
                "--dry-run",
            ],
        )

        assert result.exit_code == 0
        assert "file@example.com" in result.stdout
        assert "Hello File User" in result.stdout

    def test_batch_stdin_with_nested_data(self):
        data = [
            {
                "recipient": {"email": "sup@example.com", "preferred_name": "Sup"},
                "student": {"name": "Student One"},
            }
        ]
        result = self.runner.invoke(
            app,
            [
                "-f",
                "personal",
                "--data",
                "-",
                "--to",
                "{{recipient.email}}",
                "--subject",
                "Re: {{student.name}}",
                "--body",
                "Dear {{recipient.preferred_name}}",
                "--dry-run",
            ],
            input=json.dumps(data),
        )

        assert result.exit_code == 0
        assert "sup@example.com" in result.stdout
        assert "Re: Student One" in result.stdout
        assert "Dear Sup" in result.stdout
