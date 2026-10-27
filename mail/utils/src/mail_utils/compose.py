"""Email composition and sending utilities."""

import email
import imaplib
import mimetypes
import os
import re
import ssl
import subprocess
import sys
import tempfile
import time
from email import policy
from email.message import EmailMessage
from email.utils import formataddr, formatdate, getaddresses, make_msgid, parseaddr
from html.parser import HTMLParser
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


_HTML_BLOCKS = frozenset({"p", "div", "table", "tr", "ul", "ol", "blockquote"}) | {
    f"h{n}" for n in range(1, 7)
}
_HTML_HIDDEN = frozenset(["head", "style", "script", "title"])


class _HtmlText(HTMLParser):
    """Collects the readable text of an HTML body, one block per paragraph."""

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.chunks: list[str] = []
        self.hidden = 0
        self.href: str | None = None
        self.link_start = 0
        # Outlook wraps every <li>'s text in a <p>, so the bullet waits for
        # the item's first real text rather than going out on its own line.
        self.bullet = False
        self.in_item = 0

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag in _HTML_HIDDEN:
            self.hidden += 1
        elif tag == "br":
            self.chunks.append("\n")
        elif tag == "li":
            self._break(1)
            self.bullet = True
            self.in_item += 1
        elif tag in _HTML_BLOCKS:
            self._break(1 if self.in_item else 2)
        elif tag == "a":
            self.href = dict(attrs).get("href")
            self.link_start = len(self.chunks)

    def handle_endtag(self, tag: str) -> None:
        if tag in _HTML_HIDDEN:
            self.hidden = max(0, self.hidden - 1)
        elif tag == "li":
            self.in_item = max(0, self.in_item - 1)
        elif tag in _HTML_BLOCKS:
            self._break(1 if self.in_item else 2)
        elif tag == "a" and self.href:
            # Keep a link's target where its text doesn't already say it, or a
            # forwarded "access the folder here" points nowhere.
            text = "".join(self.chunks[self.link_start :]).strip()
            if self.href.startswith("http") and self.href != text:
                self.chunks.append(f" <{self.href}>")
            self.href = None

    def _break(self, newlines: int) -> None:
        """End the current line with at least `newlines` newlines, so nested
        blocks (<li><p>, <div><p>) don't stack up blank lines."""
        tail = re.search(r"[\n ]*$", "".join(self.chunks[-8:]))
        have = tail.group().count("\n") if tail else 0
        self.chunks.append("\n" * max(0, newlines - have))

    def handle_data(self, data: str) -> None:
        if self.hidden:
            return
        # HTML whitespace is insignificant: a source newline is a space.
        text = re.sub(r"\s+", " ", data.replace("\xa0", " "))
        if self.bullet and text.strip():
            text = "- " + text.lstrip()
            self.bullet = False
        self.chunks.append(text)


def html_to_text(html: str) -> str:
    """Plain text of an HTML body, for quoting a message that has no
    text/plain part (Outlook often sends none)."""
    parser = _HtmlText()
    parser.feed(html)
    parser.close()
    lines = [line.strip() for line in "".join(parser.chunks).splitlines()]
    return re.sub(r"\n{3,}", "\n\n", "\n".join(lines)).strip()


def parse_forward_info(message_path: Path) -> dict:
    """Extract what forwarding a message carries over.

    Returns a dict with keys: subject ("Fwd: ..."), text (a forwarded-message
    header block followed by the original body as plain text) and attachments
    (a list of (data, maintype, subtype, filename) tuples).

    Inline parts referenced by Content-ID --- signature banners and logos in
    an HTML body --- are not attachments in any sense the reader would
    recognise, and are dropped along with the HTML that showed them.
    """
    with open(message_path, "rb") as f:
        msg = email.message_from_binary_file(f, policy=policy.default)
    assert isinstance(msg, EmailMessage)

    subject = _unfold(str(msg.get("Subject", ""))) or ""
    if not re.match(r"(fwd?|fw):", subject, re.IGNORECASE):
        subject = f"Fwd: {subject}"

    part = msg.get_body(preferencelist=("plain", "html"))
    text = ""
    if isinstance(part, EmailMessage):
        content = part.get_content()
        text = (
            html_to_text(content) if part.get_content_subtype() == "html" else content
        )

    header = ["---------- Forwarded message ----------"]
    for name in ("From", "Date", "Subject", "To", "Cc"):
        value = _unfold(str(msg[name])) if msg[name] else None
        if value:
            header.append(f"{name}: {value}")

    attachments = []
    for leaf in msg.walk():
        if leaf.is_multipart() or leaf is part or leaf["Content-ID"]:
            continue
        filename = leaf.get_filename()
        if not filename:
            continue
        payload = leaf.get_payload(decode=True)
        if isinstance(payload, bytes):
            attachments.append(
                (
                    payload,
                    leaf.get_content_maintype(),
                    leaf.get_content_subtype(),
                    filename,
                )
            )

    return {
        "subject": subject,
        "text": "\n".join(header) + "\n\n" + text.strip() + "\n",
        "attachments": attachments,
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


def reply_all_cc(reply_info: dict, self_from_addr: str, to: str) -> str | None:
    """The Cc: for a reply-all: everyone on the original To and Cc, less
    ourselves and whoever the reply is already addressed to."""
    exclude = {addr.lower() for _, addr in getaddresses([self_from_addr, to])}
    everyone = join_addresses(reply_info["to"], reply_info["cc"])
    kept = [
        formataddr((name, addr))
        for name, addr in getaddresses([everyone or ""])
        if addr.lower() not in exclude
    ]
    return ", ".join(kept) if kept else None


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


def guess_attachment_type(path: Path) -> tuple[str, str]:
    """The (maintype, subtype) to attach `path` as, from its extension.

    Everything used to go out as application/octet-stream, which clients show
    as an anonymous paperclip; a real image/* or application/pdf gets an inline
    preview instead. octet-stream stays the fallback for unknown extensions.
    """
    guessed, _ = mimetypes.guess_type(path.name)
    maintype, _, subtype = (guessed or "application/octet-stream").partition("/")
    return maintype, subtype


def build_email(
    from_addr: str,
    to: str,
    subject: str,
    body: str,
    cc: str | None = None,
    attachments: list[Path] | None = None,
    reply_to: Path | None = None,
    signature: str | None = None,
    forward: Path | None = None,
) -> EmailMessage:
    """Build an email message with proper headers.

    With `forward`, the original goes inline below the body and its
    attachments are carried over. The signature still comes last, as in
    neomutt's own inline forward: placed above the forwarded text, its "-- "
    delimiter would have clients fold everything after it away.
    """
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

    forwarded = parse_forward_info(forward) if forward else None
    if forwarded:
        body = f"{body.rstrip()}\n\n{forwarded['text']}"

    msg.set_content(append_signature(body, signature))

    for data, maintype, subtype, filename in (
        forwarded["attachments"] if forwarded else ()
    ):
        msg.add_attachment(data, maintype=maintype, subtype=subtype, filename=filename)

    # No existence check: a path that isn't there raises FileNotFoundError from
    # read_bytes() and the send dies. Skipping it silently sent the body alone,
    # so a typo'd --attach produced a mail whose text said "photo attached"
    # with nothing on it, and nothing anywhere said so.
    for attachment in attachments or ():
        maintype, subtype = guess_attachment_type(attachment)
        msg.add_attachment(
            attachment.read_bytes(),
            maintype=maintype,
            subtype=subtype,
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

    with imaplib.IMAP4_SSL(
        target.host, ssl_context=ssl.create_default_context()
    ) as imap:
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
        # The mail has gone. Reporting a failed filing as a failed send would
        # invite a retry, and the recipients would get it twice.
        try:
            append_to_sent(msg, account)
        except (
            OSError,
            RuntimeError,
            subprocess.CalledProcessError,
            imaplib.IMAP4.error,
        ) as e:
            return True, f"Sent, but filing the sent copy failed: {e}"
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
    forward: Path | None = None,
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
        config.from_addr,
        to,
        subject,
        body,
        cc,
        attachments,
        reply_to,
        signature,
        forward,
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


def join_addresses(*values: str | None) -> str | None:
    """Merge address lists into one header value, dropping repeats.

    Each value may itself be a comma-separated list. Parsing goes through
    getaddresses rather than a split on "," so that quoted display names
    like "Holt, Matthew" <m@example.com> survive intact.
    """
    seen: set[str] = set()
    merged: list[str] = []
    for name, addr in getaddresses([v for v in values if v]):
        if not addr or addr.lower() in seen:
            continue
        seen.add(addr.lower())
        merged.append(formataddr((name, addr)))
    return ", ".join(merged) if merged else None
