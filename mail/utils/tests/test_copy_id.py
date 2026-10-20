"""Tests for copying a message's Message-ID as a mu query."""

from email.message import EmailMessage

from mail_utils.cli.copy_id import mu_query


def _message(message_id: str | None) -> EmailMessage:
    msg = EmailMessage()
    msg["From"] = "sender@example.com"
    msg["Subject"] = "Test"
    if message_id is not None:
        msg["Message-ID"] = message_id
    msg.set_content("Body text.")
    return msg


def test_strips_angle_brackets():
    assert mu_query(_message("<abc.123@example.com>")) == "msgid:abc.123@example.com"


def test_missing_message_id():
    assert mu_query(_message(None)) is None
