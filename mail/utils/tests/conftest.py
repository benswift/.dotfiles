"""Shared test fixtures for mail-utils."""

import mailbox
import os
import time
from email.message import EmailMessage
from email.utils import formatdate, make_msgid
from pathlib import Path

import pytest


@pytest.fixture
def sample_email() -> EmailMessage:
    """Create a sample email message."""
    msg = EmailMessage()
    msg["From"] = "sender@example.com"
    msg["To"] = "recipient@example.com"
    msg["Subject"] = "Test Subject"
    msg["Date"] = "Mon, 01 Jan 2024 12:00:00 +0000"
    msg["Message-ID"] = "<test-message-id-12345@example.com>"
    msg.set_content("This is the body of the test email.")
    return msg


@pytest.fixture
def sample_email_bytes(sample_email: EmailMessage) -> bytes:
    """Get sample email as bytes."""
    return sample_email.as_bytes()


@pytest.fixture
def temp_maildir(tmp_path: Path) -> Path:
    """Create a temporary maildir structure with test messages."""
    maildir_path = tmp_path / "maildir"
    cur_dir = maildir_path / "cur"
    new_dir = maildir_path / "new"
    tmp_dir = maildir_path / "tmp"

    cur_dir.mkdir(parents=True)
    new_dir.mkdir(parents=True)
    tmp_dir.mkdir(parents=True)

    return maildir_path


@pytest.fixture
def populated_maildir(temp_maildir: Path) -> Path:
    """Create a maildir with several test messages."""
    mbox = mailbox.Maildir(temp_maildir, create=False)

    messages = [
        {
            "from": "alice@example.com",
            "to": "bob@example.com",
            "subject": "First message",
            "message_id": "<msg1@example.com>",
            "date": "Mon, 01 Jan 2024 10:00:00 +0000",
            "body": "Body of first message",
        },
        {
            "from": "charlie@example.com",
            "to": "bob@example.com",
            "subject": "Second message",
            "message_id": "<msg2@example.com>",
            "date": "Tue, 02 Jan 2024 11:00:00 +0000",
            "body": "Body of second message",
        },
        {
            "from": "alice@example.com",
            "to": "bob@example.com",
            "subject": "Third message",
            "message_id": "<msg3@example.com>",
            "date": "Wed, 03 Jan 2024 12:00:00 +0000",
            "body": "Body of third message",
        },
    ]

    for msg_data in messages:
        msg = EmailMessage()
        msg["From"] = msg_data["from"]
        msg["To"] = msg_data["to"]
        msg["Subject"] = msg_data["subject"]
        msg["Message-ID"] = msg_data["message_id"]
        msg["Date"] = msg_data["date"]
        msg.set_content(msg_data["body"])
        mbox.add(msg)

    mbox.flush()
    mbox.close()

    return temp_maildir


@pytest.fixture
def maildir_with_duplicates(temp_maildir: Path) -> Path:
    """Create a maildir with duplicate messages (same Message-ID)."""
    mbox = mailbox.Maildir(temp_maildir, create=False)

    for i in range(3):
        msg = EmailMessage()
        msg["From"] = "sender@example.com"
        msg["To"] = "recipient@example.com"
        msg["Subject"] = f"Duplicate message (copy {i + 1})"
        msg["Message-ID"] = "<duplicate-id@example.com>"
        msg["Date"] = f"Mon, 0{i + 1} Jan 2024 10:00:00 +0000"
        msg.set_content(f"Body of duplicate {i + 1}")
        mbox.add(msg)

    msg = EmailMessage()
    msg["From"] = "other@example.com"
    msg["To"] = "recipient@example.com"
    msg["Subject"] = "Unique message"
    msg["Message-ID"] = "<unique-id@example.com>"
    msg["Date"] = "Thu, 04 Jan 2024 10:00:00 +0000"
    msg.set_content("Body of unique message")
    mbox.add(msg)

    mbox.flush()
    mbox.close()

    return temp_maildir
