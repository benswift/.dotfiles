"""Email composition and sending utilities."""

import email
import imaplib
import os
import re
import subprocess
import sys
import tempfile
import time
from email.message import EmailMessage
from email.utils import formatdate, make_msgid, parseaddr
from pathlib import Path
from typing import IO, Any

from mail_utils.accounts import Account, get_account_config


def _unfold(value: str | None) -> str | None:
    """Collapse folded header whitespace (CR/LF + leading WSP) into single spaces."""
    if value is None:
        return None
    return re.sub(r"\s+", " ", value).strip()


def parse_reply_info(message_path: Path) -> dict:
    """Extract threading and recipient info from an email file.

    Returns a dict with keys: message_id, references, from_, to, cc, subject.
    """
    with open(message_path, "rb") as f:
        msg = email.message_from_binary_file(f)

    message_id = _unfold(msg["Message-ID"])
    existing_refs = _unfold(msg.get("References", "")) or ""
    if existing_refs:
        references = f"{existing_refs} {message_id}"
    else:
        in_reply_to = _unfold(msg.get("In-Reply-To", "")) or ""
        references = (
            f"{in_reply_to} {message_id}".strip() if in_reply_to else message_id
        )

    subject = _unfold(msg.get("Subject", "")) or ""
    if not subject.lower().startswith("re:"):
        subject = f"Re: {subject}"

    return {
        "message_id": message_id,
        "references": references,
        "from_": _unfold(msg["From"]),
        "to": _unfold(msg["To"]),
        "cc": _unfold(msg.get("Cc")),
        "reply_to_header": _unfold(msg.get("Reply-To")),
        "subject": subject,
    }


def choose_reply_target(reply_info: dict, self_from_addr: str) -> str | None:
    """Pick the To: address for a reply.

    If the source message was sent by us (From matches our account), return
    the original To so the thread continues to the same recipient (e.g.
    nudging someone we already emailed). Otherwise honour Reply-To if set,
    else From.
    """
    from email.utils import getaddresses, parseaddr

    def _addr(header: str | None) -> str:
        return (parseaddr(header or "")[1] or "").lower()

    self_addr = _addr(self_from_addr)
    from_addrs = {
        addr.lower() for _, addr in getaddresses([reply_info["from_"] or ""]) if addr
    }
    if self_addr and self_addr in from_addrs:
        return reply_info["to"]
    return reply_info.get("reply_to_header") or reply_info["from_"]


# `set signature = ~/.config/neomutt/signature-anu`, optionally quoted.
SIGNATURE_SETTING = re.compile(
    r'^\s*set\s+signature\s*=\s*"?([^"\n]+?)"?\s*$', re.MULTILINE
)


def read_signature(account: Account | str) -> str | None:
    """The signature text neomutt appends for this account, if any.

    Read out of the neomutt account config rather than recorded a second
    time here, so `set signature` stays the single source of truth and an
    interactive neomutt and mail-compose can't disagree about what gets
    appended. Returns None when the account sets no signature, or the file
    it names is missing.
    """
    config = get_account_config(account)
    if not config.neomutt_config.exists():
        return None

    match = SIGNATURE_SETTING.search(config.neomutt_config.read_text())
    if not match:
        return None

    path = Path(match.group(1)).expanduser()
    if not path.exists():
        return None

    return path.read_text().strip() or None


def append_signature(body: str, signature: str | None) -> str:
    """Append a signature to a body, delimited the way neomutt does it.

    "-- " (with the trailing space) is the conventional delimiter mail
    clients use to fold a signature away, and matches $sig_dashes.
    """
    if not signature:
        return body
    return f"{body.rstrip()}\n\n-- \n{signature}\n"


def msgid_domain(from_addr: str) -> str | None:
    """The domain to stamp into a Message-ID: the From address's own.

    make_msgid() otherwise falls back to socket.getfqdn(), which here is a
    Tailscale name that doesn't resolve publicly. A Message-ID domain that
    neither resolves nor matches From is a spam signal for some filters.
    """
    _, addr = parseaddr(from_addr)
    if "@" not in addr:
        return None
    return addr.rsplit("@", 1)[1] or None


def build_email(
    from_addr: str,
    to: str,
    subject: str,
    body: str,
    cc: str | None = None,
    attachments: list[Path] | None = None,
    reply_to: Path | None = None,
    signature: str | None = None,
) -> EmailMessage:
    """Build an email message with proper headers."""
    msg = EmailMessage()
    msg["From"] = from_addr
    msg["To"] = to
    msg["Subject"] = subject
    msg["Date"] = formatdate(localtime=True)
    msg["Message-ID"] = make_msgid(domain=msgid_domain(from_addr))

    if reply_to:
        info = parse_reply_info(reply_to)
        msg["In-Reply-To"] = info["message_id"]
        msg["References"] = info["references"]

    if cc:
        msg["Cc"] = cc

    msg.set_content(append_signature(body, signature))

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


MAIL_SECRET = Path.home() / ".dotfiles/bin/mail-secret"


def append_to_sent(msg: EmailMessage, account: Account) -> None:
    """File a copy of a sent message in the account's sent folder on the
    server, for accounts whose server doesn't do that itself (see
    AccountConfig.sent_append). Deliberately not a write into ~/Maildir:
    that only reaches the server from a host that runs mbsync in push mode.
    """
    config = get_account_config(account)
    target = config.sent_append
    if target is None:
        return

    password = subprocess.run(
        [str(MAIL_SECRET), "get", target.secret_account, target.secret_service],
        capture_output=True,
        text=True,
        check=True,
    ).stdout.rstrip("\n")

    with imaplib.IMAP4_SSL(target.host) as imap:
        imap.login(target.user, password)
        status, detail = imap.append(
            f'"{config.sent_folder}"',
            r"(\Seen)",
            imaplib.Time2Internaldate(time.time()),
            msg.as_bytes(),
        )
    if status != "OK":
        raise RuntimeError(f"IMAP APPEND to {config.sent_folder} failed: {detail}")


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
        ["msmtp", "-t", "-a", config.msmtp],
        input=msg.as_bytes(),
        capture_output=True,
        check=False,
    )

    if result.returncode == 0:
        append_to_sent(msg, account)
        return True, "Sent successfully"
    else:
        return False, f"Failed to send: {result.stderr.decode()}"


def _is_terminal(stream: IO[Any] | None) -> bool:
    """Is this stream a terminal? False for anything that cannot say."""
    try:
        return stream is not None and stream.isatty()
    except (AttributeError, ValueError, OSError):
        return False


def _open_terminal() -> IO[bytes] | None:
    """The controlling terminal, or None where the process has none."""
    try:
        return open("/dev/tty", "r+b", buffering=0)
    except OSError:
        return None


def open_neomutt_compose(
    account: Account,
    to: str,
    subject: str,
    body: str,
    cc: str | None = None,
    attachments: list[Path] | None = None,
    reply_to: Path | None = None,
    signature: str | None = None,
) -> None:
    """Open neomutt in compose mode with a fully-built draft.

    The draft is the same message build_email() hands to msmtp for --send,
    so the two paths can't drift: threading headers, Cc and MIME
    attachments are all built once, here, and neomutt only transmits.
    Writing a bare To/Subject stub instead --- as this used to --- silently
    dropped In-Reply-To/References, so an interactive reply started a new
    thread while the same command with --send threaded correctly.

    $resume_draft_files stops neomutt treating the draft as a fresh
    message: without it, it re-prompts for recipients it already has, and
    appends $signature on top of the one build_email has already added. It
    also disables mutt alias expansion, which costs nothing here because
    recipients are resolved before neomutt ever sees them.
    """
    config = get_account_config(account)
    msg = build_email(
        config.from_addr, to, subject, body, cc, attachments, reply_to, signature
    )

    with tempfile.NamedTemporaryFile(suffix=".eml", delete=False) as draft_file:
        draft_file.write(msg.as_bytes())
        draft_path = Path(draft_file.name)

    cmd = [
        "neomutt",
        "-e",
        f"source {config.neomutt_config}",
        "-e",
        "set resume_draft_files=yes",
        "-H",
        str(draft_path),
    ]

    env = os.environ.copy()
    env["TERM"] = "xterm-direct"

    # neomutt is a full-screen program, so every standard stream it inherits
    # has to be the terminal. `--body -` reads the body from stdin, which by
    # the time we get here is a drained pipe: neomutt given that exits at once
    # and the draft is never shown, so the two documented ways of using this
    # command could not be combined. Hand it the controlling terminal for any
    # stream that is not already one. Where there is no controlling terminal
    # (a cron job, an agent shell) nothing can rescue an interactive compose,
    # so inherit as before and let neomutt report it.
    inherited = (sys.stdin, sys.stdout, sys.stderr)
    terminal = None if all(map(_is_terminal, inherited)) else _open_terminal()

    def channel(stream: IO[Any] | None) -> IO[bytes] | None:
        """The terminal for a stream that isn't one; None to inherit."""
        return None if terminal is None or _is_terminal(stream) else terminal

    try:
        subprocess.run(
            cmd,
            env=env,
            check=False,
            stdin=channel(sys.stdin),
            stdout=channel(sys.stdout),
            stderr=channel(sys.stderr),
        )
    finally:
        if terminal is not None:
            terminal.close()
        draft_path.unlink(missing_ok=True)


FRONTMATTER_PATTERN = re.compile(r"^---\s*\n.*?\n---\s*\n", re.DOTALL)


def strip_frontmatter(content: str) -> str:
    """Strip YAML frontmatter from markdown content."""
    return FRONTMATTER_PATTERN.sub("", content)


def combine_cc(cc: str | None, cc_all: str | None) -> str | None:
    """Combine per-message CC and global CC recipients."""
    parts = [addr for addr in (cc, cc_all) if addr]
    return ", ".join(parts) if parts else None
