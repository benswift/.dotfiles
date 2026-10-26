"""Copy Exchange mail into a local cold store before ANU's retention expunges it.

ANU deletes mail from every Exchange mailbox two years after it arrives, and
mbsync mirrors each deletion into the local maildir, on daysy and weddle alike.
Anything worth keeping has to be copied out of the mbsync-managed folders
first, into a tree no mbsync channel touches: the cold store,
``<cold>/<account>/<folder>``.

Copy, never move: the mbsync copy stays put until the server expunges it, so a
sweep changes nothing mbsync can see. A message is captured once its Date is
older than MIN_AGE, a few months short of the two years so that a laptop asleep
for weeks still gets there first, and only once per account (keyed by
Message-ID), so a re-run is a no-op. Between capture and expunge a message
exists twice; ``mu find --skip-dups`` hides the overlap.
"""

import hashlib
import re
import shutil
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from email.message import Message
from email.parser import BytesHeaderParser
from email.policy import compat32
from email.utils import parsedate_to_datetime
from pathlib import Path

from mail_utils.maildir import get_all_message_files

EXCHANGE_ACCOUNTS = ("anu", "phdconvenor", "adirh", "lxconvenor")
# Deleted Items, Junk and Drafts are left to expire with the server.
FOLDERS = ("INBOX", "Archive", "Sent Items")
MIN_AGE = timedelta(days=640)  # ~21 months, against ANU's 24
MAILDIR = Path.home() / "Maildir"
COLD_ROOT = MAILDIR / "anu-archive"


@dataclass(frozen=True)
class Capture:
    source: Path
    dest: Path


def read_headers(path: Path) -> Message:
    """Parse just the header block, without reading the body off disk."""
    lines = []
    with path.open("rb") as f:
        for line in f:
            if line in (b"\n", b"\r\n"):
                break
            lines.append(line)
    return BytesHeaderParser(policy=compat32).parsebytes(b"".join(lines))


def message_key(headers: Message, path: Path) -> str:
    """Message-ID, or a content hash for the rare message without one."""
    message_id = headers.get("Message-ID")
    if message_id:
        return str(message_id).strip().strip("<>").strip()
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


def message_date(headers: Message) -> datetime | None:
    try:
        date = parsedate_to_datetime(str(headers.get("Date", "")))
    except (TypeError, ValueError, IndexError):
        return None
    return date if date.tzinfo else date.replace(tzinfo=UTC)


def cold_name(filename: str) -> str:
    """Drop mbsync's ``,U=<uid>``: it names a server UID the cold store has none of."""
    return re.sub(r",U=\d+", "", filename)


def readable(paths: Iterable[Path]) -> Iterator[tuple[Path, Message]]:
    """Each message with its headers, skipping files that vanish mid-scan.

    mailsync runs every five minutes, and a flag change renames a file, so a
    path listed a moment ago can be gone; the next sweep sees its new name.
    """
    for path in sorted(paths):
        try:
            yield path, read_headers(path)
        except FileNotFoundError:
            continue


def cold_keys(account_root: Path) -> set[str]:
    """Keys of everything already in one account's cold store, every folder."""
    if not account_root.is_dir():
        return set()
    paths = (
        path
        for folder in account_root.iterdir()
        for path in get_all_message_files(folder)
    )
    return {message_key(headers, path) for path, headers in readable(paths)}


def plan(
    source_root: Path,
    cold_root: Path,
    now: datetime,
    min_age: timedelta = MIN_AGE,
    accounts: Iterable[str] = EXCHANGE_ACCOUNTS,
    folders: Iterable[str] = FOLDERS,
) -> list[Capture]:
    """Every message under source_root due for the cold store and not yet in it.

    A message with no parseable Date is captured regardless of age: an early
    copy costs one duplicate, a late one costs the message.
    """
    cutoff = now - min_age
    captures = []
    for account in accounts:
        kept = cold_keys(cold_root / account)
        for folder in folders:
            files = get_all_message_files(source_root / account / folder)
            for path, headers in readable(files):
                date = message_date(headers)
                if date is not None and date > cutoff:
                    continue
                key = message_key(headers, path)
                if key in kept:
                    continue
                kept.add(key)
                dest = cold_root / account / folder / "cur" / cold_name(path.name)
                captures.append(Capture(path, dest))
    return captures


def capture(item: Capture) -> bool:
    """Copy one message in via tmp/, as maildir delivery requires.

    Returns False if the source vanished before it could be copied.
    """
    maildir = item.dest.parent.parent
    for subdir in ("cur", "new", "tmp"):
        (maildir / subdir).mkdir(parents=True, exist_ok=True)
    if item.dest.exists():
        raise FileExistsError(item.dest)
    tmp = maildir / "tmp" / item.dest.name
    try:
        shutil.copy2(item.source, tmp)
    except FileNotFoundError:
        return False
    tmp.rename(item.dest)
    return True
