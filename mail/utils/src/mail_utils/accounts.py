"""Email account configuration."""

from dataclasses import dataclass
from enum import Enum
from pathlib import Path


class Account(str, Enum):
    personal = "personal"
    anu = "anu"
    phdconvenor = "phdconvenor"


@dataclass
class AccountConfig:
    from_addr: str
    msmtp: str
    maildir: Path
    sent_folder: str
    neomutt_config: Path


ACCOUNTS: dict[Account, AccountConfig] = {
    Account.personal: AccountConfig(
        from_addr="Ben Swift <ben@benswift.me>",
        msmtp="personal",
        maildir=Path.home() / "Maildir/personal",
        sent_folder="Sent Items",
        neomutt_config=Path.home() / ".config/neomutt/accounts/personal",
    ),
    Account.anu: AccountConfig(
        from_addr="Ben Swift <ben.swift@anu.edu.au>",
        msmtp="anu",
        maildir=Path.home() / "Maildir/anu",
        sent_folder="Sent Items",
        neomutt_config=Path.home() / ".config/neomutt/accounts/anu",
    ),
    Account.phdconvenor: AccountConfig(
        from_addr="Ben Swift <phdconvenor.cybernetics@anu.edu.au>",
        msmtp="phdconvenor",
        maildir=Path.home() / "Maildir/phdconvenor",
        sent_folder="Sent Items",
        neomutt_config=Path.home() / ".config/neomutt/accounts/phdconvenor",
    ),
}


def get_account_config(account: Account | str) -> AccountConfig:
    """Get configuration for an account."""
    if isinstance(account, str):
        account = Account(account)
    return ACCOUNTS[account]
