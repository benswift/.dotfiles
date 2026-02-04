"""Email parsing utilities."""

import email
import sys
from datetime import datetime
from email.message import EmailMessage
from email.policy import default
from email.utils import parsedate_to_datetime

from dateutil import parser as date_parser


def read_email_from_stdin() -> EmailMessage:
    """Read and parse an email message from stdin."""
    return email.message_from_file(sys.stdin, policy=default)


def read_email_from_file(path: str) -> EmailMessage:
    """Read and parse an email message from a file."""
    with open(path, "rb") as f:
        return email.message_from_binary_file(f, policy=default)


def read_email_from_bytes(data: bytes) -> EmailMessage:
    """Read and parse an email message from bytes."""
    return email.message_from_bytes(data, policy=default)


def get_message_id(msg: EmailMessage) -> str | None:
    """Extract the Message-ID from an email, stripped of angle brackets."""
    message_id = msg.get("Message-ID")
    if not message_id:
        return None
    return message_id.strip().strip("<>").strip()


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
