"""Email composition and sending utilities."""

import os
import re
import subprocess
import tempfile
import time
from email.message import EmailMessage
from email.utils import formatdate, make_msgid
from pathlib import Path
from socket import gethostname

from mail_utils.accounts import Account, get_account_config


def build_email(
    from_addr: str,
    to: str,
    subject: str,
    body: str,
    cc: str | None = None,
    attachments: list[Path] | None = None,
) -> EmailMessage:
    """Build an email message with proper headers."""
    msg = EmailMessage()
    msg["From"] = from_addr
    msg["To"] = to
    msg["Subject"] = subject
    msg["Date"] = formatdate(localtime=True)
    msg["Message-ID"] = make_msgid()

    if cc:
        msg["Cc"] = cc

    msg.set_content(body)

    if attachments:
        for attachment in attachments:
            if attachment.exists():
                content = attachment.read_bytes()
                msg.add_attachment(
                    content,
                    maintype="application",
                    subtype="octet-stream",
                    filename=attachment.name,
                )

    return msg


def save_to_sent(msg: EmailMessage, account: Account) -> Path:
    """Save a sent message to the account's sent folder."""
    config = get_account_config(account)
    sent_dir = config.maildir / config.sent_folder / "cur"
    sent_dir.mkdir(parents=True, exist_ok=True)

    timestamp = int(time.time())
    hostname = gethostname().split(".")[0]
    filename = f"{timestamp}.{os.getpid()}.{hostname}:2,S"
    filepath = sent_dir / filename

    filepath.write_bytes(msg.as_bytes())
    return filepath


def send_email(
    msg: EmailMessage, account: Account, dry_run: bool = False
) -> tuple[bool, str]:
    """Send an email via msmtp.

    Returns (success, message) tuple.
    """
    config = get_account_config(account)

    if dry_run:
        return True, f"Would send via msmtp account '{config.msmtp}'"

    result = subprocess.run(
        ["msmtpq", "-t", "-a", config.msmtp],
        input=msg.as_bytes(),
        capture_output=True,
    )

    if result.returncode == 0:
        save_to_sent(msg, account)
        return True, "Sent successfully"
    else:
        return False, f"Failed to send: {result.stderr.decode()}"


def open_neomutt_compose(
    account: Account,
    to: str,
    subject: str,
    body: str,
    cc: str | None = None,
    attachments: list[Path] | None = None,
) -> None:
    """Open neomutt in compose mode with a draft."""
    config = get_account_config(account)

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".eml", delete=False
    ) as draft_file:
        draft_file.write(f"To: {to}\n")
        if cc:
            draft_file.write(f"Cc: {cc}\n")
        draft_file.write(f"Subject: {subject}\n\n")
        draft_file.write(body)
        draft_path = Path(draft_file.name)

    cmd = ["neomutt", "-e", f"source {config.neomutt_config}", "-H", str(draft_path)]

    if attachments:
        for attachment in attachments:
            if attachment.exists():
                cmd.extend(["-a", str(attachment)])
        cmd.append("--")

    env = os.environ.copy()
    env["TERM"] = "xterm-direct"

    try:
        subprocess.run(cmd, env=env)
    finally:
        draft_path.unlink(missing_ok=True)


FRONTMATTER_PATTERN = re.compile(r"^---\s*\n.*?\n---\s*\n", re.DOTALL)


def strip_frontmatter(content: str) -> str:
    """Strip YAML frontmatter from markdown content."""
    return FRONTMATTER_PATTERN.sub("", content)


def combine_cc(cc: str | None, cc_all: str | None) -> str | None:
    """Combine per-message CC and global CC recipients."""
    parts = [addr for addr in (cc, cc_all) if addr]
    return ", ".join(parts) if parts else None
