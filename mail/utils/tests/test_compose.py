"""Tests for email composition utilities."""

import json
from pathlib import Path
from unittest.mock import MagicMock, patch

from typer.testing import CliRunner

from mail_utils.accounts import Account, get_account_config
from mail_utils.cli.compose import app
from mail_utils.compose import (
    build_email,
    choose_reply_target,
    combine_cc,
    parse_reply_info,
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


def _write_eml(path: Path, **headers: str) -> Path:
    """Write a minimal .eml file with given headers."""
    lines = [f"{k}: {v}" for k, v in headers.items()]
    lines.append("")
    lines.append("Original body.")
    path.write_text("\n".join(lines))
    return path


class TestParseReplyInfo:
    def test_extracts_message_id_and_subject(self, tmp_path: Path):
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Subject="Hello",
            **{"Message-ID": "<abc123@example.com>"},
        )
        info = parse_reply_info(eml)
        assert info["message_id"] == "<abc123@example.com>"
        assert info["subject"] == "Re: Hello"
        assert info["to"] == "Bob <bob@example.com>"
        assert info["from_"] == "Alice <alice@example.com>"

    def test_preserves_existing_re_prefix(self, tmp_path: Path):
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Subject="Re: Hello",
            **{"Message-ID": "<abc123@example.com>"},
        )
        info = parse_reply_info(eml)
        assert info["subject"] == "Re: Hello"

    def test_builds_references_from_existing_chain(self, tmp_path: Path):
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Subject="Re: Hello",
            References="<first@example.com> <second@example.com>",
            **{
                "Message-ID": "<third@example.com>",
                "In-Reply-To": "<second@example.com>",
            },
        )
        info = parse_reply_info(eml)
        assert info["references"] == "<first@example.com> <second@example.com> <third@example.com>"

    def test_builds_references_from_in_reply_to_when_no_references(self, tmp_path: Path):
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Subject="Hello",
            **{
                "Message-ID": "<second@example.com>",
                "In-Reply-To": "<first@example.com>",
            },
        )
        info = parse_reply_info(eml)
        assert info["references"] == "<first@example.com> <second@example.com>"

    def test_extracts_cc(self, tmp_path: Path):
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Cc="Carol <carol@example.com>",
            Subject="Hello",
            **{"Message-ID": "<abc@example.com>"},
        )
        info = parse_reply_info(eml)
        assert info["cc"] == "Carol <carol@example.com>"

    def test_unfolds_multiline_headers(self, tmp_path: Path):
        """Folded headers (RFC 5322 line folding) must be unfolded so the
        resulting values can be used as outgoing header values without
        tripping EmailMessage's no-linefeed check."""
        path = tmp_path / "folded.eml"
        path.write_text(
            "From: Alice <alice@example.com>\n"
            "To: =?utf-8?Q?Bob?=\n"
            "\t<bob@example.com>\n"
            "Subject: Hello\n"
            "Message-ID: <third@example.com>\n"
            "References: <first@example.com>\n"
            " <second@example.com>\n"
            "\n"
            "Body.\n"
        )
        info = parse_reply_info(path)
        for key in ("message_id", "references", "from_", "to", "subject"):
            assert "\n" not in info[key], f"{key} contains newline: {info[key]!r}"
        assert info["references"] == (
            "<first@example.com> <second@example.com> <third@example.com>"
        )
        # And the result should be usable as an outgoing header value.
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
            reply_to=path,
        )
        assert msg["References"] == info["references"]


class TestChooseReplyTarget:
    def test_incoming_mail_targets_from(self):
        info = {
            "from_": "Alice <alice@example.com>",
            "to": "Bob <bob@example.com>",
            "reply_to_header": None,
        }
        assert choose_reply_target(info, "Bob <bob@example.com>") == "Alice <alice@example.com>"

    def test_own_sent_mail_continues_thread(self):
        """Replying to our own sent message should go to its original To,
        so nudges thread correctly to the same recipient."""
        info = {
            "from_": "Bob <bob@example.com>",
            "to": "Alice <alice@example.com>",
            "reply_to_header": None,
        }
        assert choose_reply_target(info, "Bob <bob@example.com>") == "Alice <alice@example.com>"

    def test_reply_to_header_overrides_from(self):
        info = {
            "from_": "Alice <alice@example.com>",
            "to": "Bob <bob@example.com>",
            "reply_to_header": "Alice Team <team@example.com>",
        }
        assert (
            choose_reply_target(info, "Bob <bob@example.com>")
            == "Alice Team <team@example.com>"
        )

    def test_self_match_is_case_insensitive(self):
        info = {
            "from_": "Bob <BOB@example.com>",
            "to": "Alice <alice@example.com>",
            "reply_to_header": None,
        }
        assert choose_reply_target(info, "Bob <bob@example.com>") == "Alice <alice@example.com>"


class TestBuildEmailReply:
    def test_sets_threading_headers(self, tmp_path: Path):
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Subject="Hello",
            **{"Message-ID": "<orig@example.com>"},
        )
        msg = build_email(
            from_addr="bob@example.com",
            to="alice@example.com",
            subject="Re: Hello",
            body="Reply body",
            reply_to=eml,
        )
        assert msg["In-Reply-To"] == "<orig@example.com>"
        assert "<orig@example.com>" in msg["References"]

    def test_no_threading_headers_without_reply_to(self):
        msg = build_email(
            from_addr="bob@example.com",
            to="alice@example.com",
            subject="Hello",
            body="Body",
        )
        assert msg["In-Reply-To"] is None
        assert msg["References"] is None


class TestComposeCLIReplyTo:
    runner = CliRunner()

    def test_reply_to_auto_populates_subject(self, tmp_path: Path):
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Subject="Meeting",
            **{"Message-ID": "<orig@example.com>"},
        )
        result = self.runner.invoke(
            app,
            ["-f", "personal", "--reply-to", str(eml), "--to", "alice@example.com", "-b", "Sounds good", "--dry-run"],
        )
        assert result.exit_code == 0
        assert "Re: Meeting" in result.stdout
        assert "In-Reply-To: <orig@example.com>" in result.stdout
        assert "References:" in result.stdout

    def test_reply_to_auto_populates_to_from_sender(self, tmp_path: Path):
        """Incoming mail: To defaults to the original sender (From)."""
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Subject="Meeting",
            **{"Message-ID": "<orig@example.com>"},
        )
        result = self.runner.invoke(
            app,
            ["-f", "personal", "--reply-to", str(eml), "-b", "Sounds good", "--dry-run"],
        )
        assert result.exit_code == 0
        assert "alice@example.com" in result.stdout

    def test_reply_to_own_sent_mail_continues_thread(self, tmp_path: Path):
        """Replying to our own sent mail: To defaults to original To
        (so nudges thread to the same recipient)."""
        from mail_utils.accounts import get_account_config

        self_addr = get_account_config("personal").from_addr
        eml = _write_eml(
            tmp_path / "orig.eml",
            From=self_addr,
            To="Alice <alice@example.com>",
            Subject="Following up",
            **{"Message-ID": "<orig@example.com>"},
        )
        result = self.runner.invoke(
            app,
            ["-f", "personal", "--reply-to", str(eml), "-b", "Just a nudge", "--dry-run"],
        )
        assert result.exit_code == 0
        assert "alice@example.com" in result.stdout

    def test_explicit_to_overrides_reply_to(self, tmp_path: Path):
        eml = _write_eml(
            tmp_path / "orig.eml",
            From="Alice <alice@example.com>",
            To="Bob <bob@example.com>",
            Subject="Meeting",
            **{"Message-ID": "<orig@example.com>"},
        )
        result = self.runner.invoke(
            app,
            ["-f", "personal", "--reply-to", str(eml), "--to", "someone-else@example.com", "-b", "Hi", "--dry-run"],
        )
        assert result.exit_code == 0
        assert "someone-else@example.com" in result.stdout
