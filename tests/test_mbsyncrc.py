"""Guard the pull-only mbsync configs that weddle and watson run.

isync has no include, so mbsyncrc-weddle and mbsyncrc-wiz restate the
accounts from mbsyncrc. These checks keep them from drifting and, above all,
keep every channel in them from ever writing to a server: daysy must stay
the only pushing client (mail/README.md).
"""

from pathlib import Path

import pytest

MAIL = Path(__file__).resolve().parents[1] / "mail"
PULL_ONLY = ("mbsyncrc-weddle", "mbsyncrc-wiz")
# PassCmd differs on purpose (watson forces the file backend), and wiz
# leaves MaxSize out because it never uploads.
ACCOUNT_KEYS = ("Host", "Port", "AuthMech", "User", "TLSType")


def sections(name: str) -> dict[tuple[str, str], dict[str, str]]:
    """Each top-level section, keyed by (kind, name), as key -> value."""
    out: dict[tuple[str, str], dict[str, str]] = {}
    current: dict[str, str] | None = None
    for raw in (MAIL / name).read_text().splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            current = None if not line else current
            continue
        key, _, value = line.partition(" ")
        if current is None:
            current = out.setdefault((key, value), {})
        else:
            current[key] = value
    return out


def channels(name: str) -> dict[str, dict[str, str]]:
    return {n: s for (kind, n), s in sections(name).items() if kind == "Channel"}


@pytest.mark.parametrize("name", PULL_ONLY)
def test_every_channel_is_pull_only(name: str) -> None:
    for channel, opts in channels(name).items():
        assert opts.get("Sync") == "Pull", channel
        for key in ("Create", "Remove", "Expunge"):
            assert opts.get(key, "None") in ("Near", "None"), f"{channel} {key}"


@pytest.mark.parametrize("name", PULL_ONLY)
def test_accounts_match_the_full_config(name: str) -> None:
    full = sections("mbsyncrc")
    for (kind, account), opts in sections(name).items():
        if kind != "IMAPAccount":
            continue
        expected = full[(kind, account)]
        for key in ACCOUNT_KEYS:
            assert opts.get(key) == expected.get(key), f"{account} {key}"


def test_weddle_mirrors_every_folder_daysy_syncs() -> None:
    full = channels("mbsyncrc")
    weddle = channels("mbsyncrc-weddle")
    assert weddle.keys() == full.keys()
    for channel, opts in weddle.items():
        assert opts["Patterns"] == full[channel]["Patterns"], channel


def test_weddle_never_writes_through_the_maildir_symlink() -> None:
    stores = [
        s
        for (kind, _), s in sections("mbsyncrc-weddle").items()
        if kind == "MaildirStore"
    ]
    assert stores
    for store in stores:
        assert store["Path"].startswith("/data/Maildir-imap/")
        assert store["Inbox"].startswith("/data/Maildir-imap/")
