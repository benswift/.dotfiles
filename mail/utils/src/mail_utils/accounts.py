"""Email account configuration."""

from dataclasses import dataclass
from enum import Enum
from pathlib import Path


class Account(str, Enum):
    personal = "personal"
    anu = "anu"
    phdconvenor = "phdconvenor"


@dataclass(frozen=True)
class SentAppend:
    """Where mail-compose files its copy of a sent message: an IMAP APPEND
    straight to the server, fetched by the next mbsync like any other mail.

    Writing the copy into ~/Maildir instead only works on a host that also
    pushes (daysy); on a host that merely mirrors or pulls, a local copy is
    the only record and gets deleted by the next sync. The password comes
    from bin/mail-secret under the same (account, service) the rc files use.
    """

    host: str
    user: str
    secret_account: str
    secret_service: str


@dataclass
class AccountConfig:
    from_addr: str
    msmtp: str
    maildir: Path
    sent_folder: str
    neomutt_config: Path
    # None where the server files its own copy of anything submitted over
    # SMTP AUTH (Exchange does; see the `unset record` note in
    # mail/neomutt/accounts/anu). Fastmail doesn't, so it appends.
    sent_append: SentAppend | None


ACCOUNTS: dict[Account, AccountConfig] = {
    Account.personal: AccountConfig(
        from_addr="Ben Swift <ben@benswift.me>",
        msmtp="personal",
        maildir=Path.home() / "Maildir/personal",
        sent_folder="Sent Items",
        neomutt_config=Path.home() / ".config/neomutt/accounts/personal",
        sent_append=SentAppend(
            host="imap.fastmail.com",
            user="benswift@fastmail.com",
            secret_account="benswift@fastmail.com",
            secret_service="mbsync-fastmail",
        ),
    ),
    Account.anu: AccountConfig(
        from_addr="Ben Swift <ben.swift@anu.edu.au>",
        msmtp="anu",
        maildir=Path.home() / "Maildir/anu",
        sent_folder="Sent Items",
        neomutt_config=Path.home() / ".config/neomutt/accounts/anu",
        sent_append=None,
    ),
    Account.phdconvenor: AccountConfig(
        from_addr="Ben Swift <phdconvenor.cybernetics@anu.edu.au>",
        msmtp="phdconvenor",
        maildir=Path.home() / "Maildir/phdconvenor",
        sent_folder="Sent Items",
        neomutt_config=Path.home() / ".config/neomutt/accounts/phdconvenor",
        sent_append=None,
    ),
}


def get_account_config(account: Account | str) -> AccountConfig:
    """Get configuration for an account."""
    if isinstance(account, str):
        account = Account(account)
    return ACCOUNTS[account]
