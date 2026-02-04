"""Email utilities for maildir management."""

from mail_utils.accounts import Account, AccountConfig, get_account_config, ACCOUNTS
from mail_utils.clipboard import copy_to_clipboard
from mail_utils.compose import (
    build_email,
    send_email,
    save_to_sent,
    open_neomutt_compose,
    strip_frontmatter,
    combine_cc,
)
from mail_utils.email import parse_email_date, read_email_from_stdin, get_message_id
from mail_utils.maildir import (
    open_maildir,
    is_maildir,
    extract_flags,
    extract_uid,
    generate_filename,
)
from mail_utils.mu import find_message_path

__all__ = [
    "Account",
    "AccountConfig",
    "ACCOUNTS",
    "get_account_config",
    "build_email",
    "send_email",
    "save_to_sent",
    "open_neomutt_compose",
    "strip_frontmatter",
    "combine_cc",
    "copy_to_clipboard",
    "parse_email_date",
    "read_email_from_stdin",
    "get_message_id",
    "open_maildir",
    "is_maildir",
    "extract_flags",
    "extract_uid",
    "generate_filename",
    "find_message_path",
]
