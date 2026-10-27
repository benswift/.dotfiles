"""Compose and send emails with template substitution support."""

import json
import sys
from pathlib import Path
from typing import Annotated, Any

import typer
from jinja2 import Environment, StrictUndefined
from rich.console import Console

from mail_utils.accounts import Account, get_account_config
from mail_utils.compose import (
    build_email,
    choose_reply_target,
    join_addresses,
    open_neomutt_compose,
    parse_forward_info,
    parse_reply_info,
    read_signature,
    reply_all_cc,
    send_email,
    strip_frontmatter,
)

console = Console()
app = typer.Typer(
    help="Compose and send emails with template substitution.",
    add_completion=False,
    no_args_is_help=True,
)


JINJA_ENV = Environment(undefined=StrictUndefined)


def substitute_template(template: str, data: dict[str, Any]) -> str:
    """Render a Jinja2 template with data."""
    return JINJA_ENV.from_string(template).render(data)


@app.command()
def main(
    account: Annotated[
        Account, typer.Option("--from", "-f", help="Account to send from")
    ],
    to: Annotated[
        list[str] | None,
        typer.Option(help="Recipient, repeatable (supports {{field}} templates)"),
    ] = None,
    cc: Annotated[
        list[str] | None,
        typer.Option(
            "--cc",
            "-c",
            help="CC recipient, repeatable (supports templates in batch mode)",
        ),
    ] = None,
    cc_all: Annotated[
        list[str] | None,
        typer.Option("--cc-all", help="CC recipient added to all emails, repeatable"),
    ] = None,
    subject: Annotated[
        str | None, typer.Option("--subject", "-s", help="Subject line")
    ] = None,
    body: Annotated[
        str | None,
        typer.Option("--body", "-b", help="Body text (or '-' for stdin)"),
    ] = None,
    template: Annotated[
        Path | None, typer.Option(help="Template file for body")
    ] = None,
    attach: Annotated[
        list[Path] | None, typer.Option("--attach", "-a", help="Attachment")
    ] = None,
    reply_to: Annotated[
        Path | None,
        typer.Option("--reply-to", help="Maildir message file to reply to"),
    ] = None,
    reply_all: Annotated[
        bool,
        typer.Option(
            "--reply-all",
            help="With --reply-to, also Cc everyone on the original To and Cc",
        ),
    ] = False,
    forward: Annotated[
        Path | None,
        typer.Option(
            "--forward",
            help="Maildir message file to forward inline, with its attachments",
        ),
    ] = None,
    data: Annotated[
        Path | None,
        typer.Option(help="JSON array file for batch mode (use '-' for stdin)"),
    ] = None,
    send: Annotated[bool, typer.Option(help="Send directly without editor")] = False,
    signature: Annotated[
        bool,
        typer.Option(
            help="Append the account's neomutt signature to the body",
        ),
    ] = True,
    dry_run: Annotated[bool, typer.Option(help="Preview without sending")] = False,
) -> None:
    """
    Compose and send emails with template substitution.

    Examples:

        # Interactive compose (opens neomutt)
        mail-compose -f anu --to colleague@example.com -s "Hello" -b "Body"

        # Direct send (--to, --cc and --cc-all repeat; each also takes a
        # comma-separated list)
        mail-compose -f anu --to colleague@example.com -c a@example.com \\
            -c b@example.com -s "Hello" -b "Body" --send

        # Reply to an existing message (auto-fills To, Subject and the
        # original Cc; --reply-all also copies the original To recipients)
        msg=$(mu find --fields=l "from:chris subject:Meeting" | head -1)
        mail-compose -f anu --reply-to "$msg" -b "Sounds good" --send

        # Forward a message inline (Subject defaults to "Fwd: ..."; the body
        # goes above the forwarded text, and batch mode works too)
        mail-compose -f anu --forward "$msg" --to alice@example.com \\
            -b "FYI" --send

        # Batch with template (filter with jq before piping)
        student-db students --status confirmed | \\
            mail-compose -f phdconvenor --data - --to '{{email}}' \\
            --subject 'Hello {{preferred_name}}' --template body.md --send
    """
    config = get_account_config(account)
    # Resolved once here rather than per-message: the signature supplies
    # the sign-off, so bodies and templates should not carry one of their
    # own. --no-signature covers the exceptions.
    sig = read_signature(account) if signature else None

    if body == "-" and data and str(data) == "-":
        console.print("[red]Cannot read both --body and --data from stdin[/red]")
        raise typer.Exit(1)

    email_body = ""
    if template:
        if not template.exists():
            console.print(f"[red]Template file not found: {template}[/red]")
            raise typer.Exit(1)
        email_body = strip_frontmatter(template.read_text())
    elif body == "-":
        email_body = sys.stdin.read()
    elif body:
        email_body = body

    if reply_all and not reply_to:
        console.print("[red]--reply-all needs --reply-to[/red]")
        raise typer.Exit(1)

    if forward and reply_to:
        console.print("[red]--forward and --reply-to are exclusive[/red]")
        raise typer.Exit(1)

    if forward:
        if not forward.exists():
            console.print(f"[red]Message to forward not found: {forward}[/red]")
            raise typer.Exit(1)
        if not subject:
            subject = parse_forward_info(forward)["subject"]

    reply_info = None
    if reply_to:
        if not reply_to.exists():
            console.print(f"[red]Reply-to message not found: {reply_to}[/red]")
            raise typer.Exit(1)
        reply_info = parse_reply_info(reply_to)
        if not to:
            target = choose_reply_target(reply_info, config.from_addr)
            to = [target] if target else None
        if not subject:
            subject = reply_info["subject"]
        if reply_all:
            others = reply_all_cc(
                reply_info, config.from_addr, join_addresses(*(to or [])) or ""
            )
            cc = [*(cc or []), *([others] if others else [])]
        elif not cc and reply_info["cc"]:
            cc = [reply_info["cc"]]

    if data:
        if str(data) == "-":
            data_text = sys.stdin.read()
        elif not data.exists():
            console.print(f"[red]Data file not found: {data}[/red]")
            raise typer.Exit(1)
        else:
            data_text = data.read_text()

        try:
            parsed = json.loads(data_text)
        except json.JSONDecodeError as e:
            console.print(f"[red]Invalid JSON: {e}[/red]")
            raise typer.Exit(1)

        if not isinstance(parsed, list):
            console.print("[red]JSON data must be an array[/red]")
            raise typer.Exit(1)

        if not all(isinstance(record, dict) for record in parsed):
            console.print("[red]JSON data must be an array of objects[/red]")
            raise typer.Exit(1)

        records: list[dict[str, Any]] = parsed

        if not records:
            console.print("No records to process")
            raise typer.Exit(0)

        if not to:
            console.print("[red]--to is required for batch mode[/red]")
            raise typer.Exit(1)

        if not subject:
            console.print("[red]--subject is required for batch mode[/red]")
            raise typer.Exit(1)

        if not send and not dry_run:
            console.print("[red]Batch mode requires --send or --dry-run[/red]")
            raise typer.Exit(1)

        sent, failed = 0, 0
        for i, record in enumerate(records, 1):
            email_to = join_addresses(*(substitute_template(t, record) for t in to))
            email_subject = substitute_template(subject, record)
            email_content = substitute_template(email_body, record)
            templated_cc = [substitute_template(c, record) for c in cc or []]
            email_cc = join_addresses(*templated_cc, *(cc_all or []))
            if not email_to:
                failed += 1
                console.print(f"[red]Record {i}: --to rendered empty[/red]")
                continue

            msg = build_email(
                config.from_addr,
                email_to,
                email_subject,
                email_content,
                email_cc,
                signature=sig,
                forward=forward,
            )

            if dry_run:
                console.print(f"=== Email {i}/{len(records)} ===")
                console.print(msg.as_string())

            success, message = send_email(msg, account, dry_run)
            if success:
                sent += 1
                if not dry_run:
                    console.print(f"Sent {i}/{len(records)}: {email_to}")
            else:
                failed += 1
                console.print(f"[red]{message}[/red]")

        if not dry_run:
            console.print(f"Sent: {sent}, Failed: {failed}")

    else:
        to_header = join_addresses(*(to or []))
        cc_header = join_addresses(*(cc or []), *(cc_all or []))
        if not to_header:
            console.print("[red]--to is required[/red]")
            raise typer.Exit(1)

        if send or dry_run:
            msg = build_email(
                config.from_addr,
                to_header,
                subject or "",
                email_body,
                cc_header,
                attach,
                reply_to,
                sig,
                forward,
            )

            success, message = send_email(msg, account, dry_run)
            if success:
                if dry_run:
                    console.print(f"{message}\n{msg.as_string()}")
                else:
                    console.print(f"Sent to {to_header}")
            else:
                console.print(f"[red]{message}[/red]")
                raise typer.Exit(1)
        else:
            open_neomutt_compose(
                account,
                to_header,
                subject or "",
                email_body,
                cc_header,
                attach,
                reply_to,
                sig,
                forward,
            )


if __name__ == "__main__":
    app()
