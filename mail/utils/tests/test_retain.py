"""Tests for the cold-store sweep, against real maildir trees on disk."""

from datetime import UTC, datetime
from email.message import EmailMessage
from pathlib import Path

from mail_utils.cli.retain import app
from mail_utils.maildir import get_all_message_files
from mail_utils.retain import capture, plan
from typer.testing import CliRunner

NOW = datetime(2026, 10, 27, tzinfo=UTC)
OLD = "Mon, 01 Jan 2024 12:00:00 +1100"
RECENT = "Mon, 01 Jun 2026 12:00:00 +1000"


def deliver(maildir: Path, name: str, message_id: str | None, date: str | None) -> Path:
    msg = EmailMessage()
    msg["From"] = "someone@example.com"
    msg["Subject"] = name
    if message_id:
        msg["Message-ID"] = message_id
    if date:
        msg["Date"] = date
    msg.set_content(f"body of {name}")
    for subdir in ("cur", "new", "tmp"):
        (maildir / subdir).mkdir(parents=True, exist_ok=True)
    path = maildir / "cur" / f"1700000000.R1.daysy,U={len(name)}:2,S{name}"
    path.write_bytes(msg.as_bytes())
    return path


def sweep(source: Path, cold: Path) -> list[Path]:
    for item in plan(source, cold, NOW):
        assert capture(item)
    return sorted(
        p.relative_to(cold)
        for account in cold.iterdir()
        for folder in account.iterdir()
        for p in get_all_message_files(folder)
    )


def test_copies_only_old_mail_and_leaves_source_alone(tmp_path: Path):
    source, cold = tmp_path / "Maildir", tmp_path / "cold"
    old = deliver(source / "anu/Sent Items", "a", "<old@x>", OLD)
    deliver(source / "anu/Sent Items", "b", "<new@x>", RECENT)

    assert sweep(source, cold) == [Path("anu/Sent Items/cur/1700000000.R1.daysy:2,Sa")]
    assert old.exists()
    copied = cold / "anu/Sent Items/cur/1700000000.R1.daysy:2,Sa"
    assert copied.read_bytes() == old.read_bytes()
    assert not any((cold / "anu/Sent Items/tmp").iterdir())


def test_rerun_is_a_no_op(tmp_path: Path):
    source, cold = tmp_path / "Maildir", tmp_path / "cold"
    deliver(source / "phdconvenor/Archive", "a", "<old@x>", OLD)
    sweep(source, cold)

    assert plan(source, cold, NOW) == []


def test_message_already_kept_under_another_folder_is_not_copied(tmp_path: Path):
    """Archived from INBOX after the INBOX copy was captured: still one copy."""
    source, cold = tmp_path / "Maildir", tmp_path / "cold"
    deliver(cold / "anu/INBOX", "a", "<moved@x>", OLD)
    deliver(source / "anu/Archive", "a", "<moved@x>", OLD)

    assert plan(source, cold, NOW) == []


def test_dedupe_is_per_account(tmp_path: Path):
    """A message cc'd to two mailboxes belongs in both accounts' archives."""
    source, cold = tmp_path / "Maildir", tmp_path / "cold"
    deliver(source / "anu/INBOX", "a", "<cc@x>", OLD)
    deliver(source / "adirh/INBOX", "a", "<cc@x>", OLD)

    assert [p.parts[0] for p in sweep(source, cold)] == ["adirh", "anu"]


def test_missing_date_is_captured_missing_id_is_deduped_by_content(tmp_path: Path):
    source, cold = tmp_path / "Maildir", tmp_path / "cold"
    deliver(source / "lxconvenor/INBOX", "nodate", "<nodate@x>", None)
    deliver(source / "lxconvenor/INBOX", "noid", None, OLD)

    assert len(sweep(source, cold)) == 2
    assert plan(source, cold, NOW) == []


def test_personal_and_unswept_folders_are_ignored(tmp_path: Path):
    source, cold = tmp_path / "Maildir", tmp_path / "cold"
    deliver(source / "personal/Archive", "a", "<p@x>", OLD)
    deliver(source / "anu/Deleted Items", "b", "<d@x>", OLD)

    assert plan(source, cold, NOW) == []


def test_cli_dry_run_copies_nothing(tmp_path: Path):
    source, cold = tmp_path / "Maildir", tmp_path / "cold"
    deliver(source / "anu/Archive", "a", "<old@x>", OLD)
    args = ["--source", str(source), "--cold", str(cold)]

    result = CliRunner().invoke(app, [*args, "--dry-run"])
    assert result.exit_code == 0, result.output
    assert "would copy 1" in result.output
    assert not cold.exists()

    result = CliRunner().invoke(app, args)
    assert "copied 1" in result.output
    assert "nothing due" in CliRunner().invoke(app, args).output
