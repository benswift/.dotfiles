"""Maildir operations."""

import mailbox
import os
import random
import re
import socket
from pathlib import Path


def is_maildir(path: Path) -> bool:
    """Check if a path looks like a maildir folder."""
    return any((path / subdir).exists() for subdir in ["cur", "new", "tmp"])


def open_maildir(path: Path, create: bool = False) -> mailbox.Maildir:
    """Open a maildir folder."""
    return mailbox.Maildir(path, create=create)


def extract_flags(filename: str) -> str:
    """Extract the flags portion from a maildir filename.

    Returns the flags string (e.g., "S", "RS", "FS") or empty string.
    """
    match = re.search(r":2,([A-Z]*)", filename)
    if match:
        return match.group(1)
    return ""


def extract_uid(filename: str) -> str | None:
    """Extract the UID from a maildir filename.

    Returns the UID number or None if not found.
    """
    match = re.search(r",U=(\d+)", filename)
    if match:
        return match.group(1)
    return None


def extract_timestamp(filename: str) -> int | None:
    """Extract the Unix timestamp from a maildir filename.

    Returns the timestamp or None if not found.
    """
    match = re.match(r"(\d{10})", filename)
    if match:
        return int(match.group(1))
    return None


def extract_hostname(filename: str) -> str | None:
    """Extract the hostname from a maildir filename.

    Handles mbsync format: timestamp.R<random>.<hostname>,U=<uid>:2,<flags>
    Returns the hostname or None if not found.
    """
    match = re.search(r"\.R?\d+\.([^,:/]+),U=", filename)
    if match:
        return match.group(1)
    return None


def is_mbsync_format(filename: str) -> bool:
    """Check if a filename uses mbsync-compatible format."""
    return bool(re.match(r"\d{10}\.R\d+\.[^,]+,U=\d+:2,", filename))


def generate_filename(
    timestamp: int,
    hostname: str | None = None,
    uid: str | None = None,
    flags: str = "",
    use_random_id: bool = True,
) -> str:
    """Generate an mbsync-compatible maildir filename.

    Format: timestamp.R<random_id>.hostname,U=<uid>:2,<flags>
    """
    if hostname is None:
        hostname = socket.gethostname().split(".")[0]

    if use_random_id:
        random_id = random.randint(10**15, 10**18 - 1)
        base = f"{timestamp}.R{random_id}.{hostname}"
    else:
        pid = os.getpid()
        seq = random.randint(1, 100000)
        base = f"{timestamp}.{pid}_{seq}.{hostname}"

    if uid:
        base = f"{base},U={uid}"

    return f"{base}:2,{flags}"


def get_all_message_files(maildir_path: Path) -> list[Path]:
    """Get all message files in a maildir's cur/ and new/ directories."""
    files = []
    for subdir in ("cur", "new"):
        d = maildir_path / subdir
        if d.exists():
            files.extend(f for f in d.iterdir() if f.is_file())
    return files
