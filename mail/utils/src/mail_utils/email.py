"""Email parsing utilities."""

import email
import re
import sys
from datetime import datetime
from email.message import EmailMessage
from email.policy import default
from email.utils import parsedate_to_datetime

from dateutil import parser as date_parser


def read_email_from_stdin() -> EmailMessage:
    """Read and parse an email message from stdin."""
    return email.message_from_binary_file(sys.stdin.buffer, policy=default)


def read_email_from_file(path: str) -> EmailMessage:
    """Read and parse an email message from a file."""
    with open(path, "rb") as f:
        return email.message_from_binary_file(f, policy=default)


def read_email_from_bytes(data: bytes) -> EmailMessage:
    """Read and parse an email message from bytes."""
    return email.message_from_bytes(data, policy=default)


def read_email_from_bytes_lenient(data: bytes) -> EmailMessage:
    """Parse email bytes, handling stripped top-level headers.

    When neomutt's <pipe-message> is used in the attach menu, it strips
    the top-level Content-Type header, leaving raw MIME body starting
    with a boundary marker. This detects that case and prepends a
    synthetic Content-Type header so the parser can handle it.
    """
    stripped = data.lstrip()
    if stripped.startswith(b"--"):
        line = stripped.split(b"\r\n", 1)[0].split(b"\n", 1)[0].strip()
        boundary = line[2:]
        header = b'Content-Type: multipart/mixed; boundary="' + boundary + b'"\r\n\r\n'
        data = header + stripped
    return email.message_from_bytes(data, policy=default)


def get_message_id(msg: EmailMessage) -> str | None:
    """Extract the Message-ID from an email, stripped of angle brackets."""
    message_id = msg.get("Message-ID")
    if not message_id:
        return None
    return message_id.strip().strip("<>").strip()


def get_attachment_filenames(msg: EmailMessage) -> list[str]:
    """Extract filenames from all attachments in an email."""
    return [fn for part in msg.walk() if (fn := part.get_filename())]


def get_attachment_filenames_raw(data: bytes) -> list[str]:
    """Extract attachment filenames from raw MIME bytes via regex.

    Fallback for when the email parser can't handle the input, e.g. when
    neomutt's <pipe-message> strips top-level headers in the attach menu.
    """
    seen: set[str] = set()
    result: list[str] = []
    for match in re.finditer(rb'filename="([^"]+)"', data):
        name = match.group(1).decode(errors="replace")
        if name not in seen:
            seen.add(name)
            result.append(name)
    return result


def parse_email_date(date_string: str) -> datetime | None:
    """Parse an email Date header into a datetime object.

    Handles various date formats found in email headers.
    Returns None if parsing fails.
    """
    if not date_string:
        return None

    try:
        return parsedate_to_datetime(date_string)
    except Exception:
        pass

    try:
        return date_parser.parse(date_string, fuzzy=True)
    except Exception:
        pass

    return None
