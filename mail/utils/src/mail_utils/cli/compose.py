"""Compose and send emails with template substitution support."""

import json
import sys
from pathlib import Path
from typing import Annotated

import typer
from jinja2 import Environment, StrictUndefined
from rich.console import Console

from mail_utils.accounts import Account, get_account_config
from mail_utils.compose import (
    build_email,
    combine_cc,
    open_neomutt_compose,
    parse_reply_info,
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


def substitute_template(template: str, data: dict) -> str:
    """Render a Jinja2 template with data."""
    return JINJA_ENV.from_string(template).render(data)


@app.command()
def main(
    account: Annotated[
        Account, typer.Option("--from", "-f", help="Account to send from")
    ],
    to: Annotated[
        str | None,
        typer.Option(help="Recipient (supports {{field}} templates)"),
    ] = None,
    cc: Annotated[
        str | None,
        typer.Option(
            "--cc", "-c", help="CC recipient (supports templates in batch mode)"
        ),
    ] = None,
    cc_all: Annotated[
        str | None, typer.Option("--cc-all", help="CC recipient added to all emails")
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
    data: Annotated[
        Path | None,
        typer.Option(help="JSON array file for batch mode (use '-' for stdin)"),
    ] = None,
    send: Annotated[bool, typer.Option(help="Send directly without editor")] = False,
    dry_run: Annotated[bool, typer.Option(help="Preview without sending")] = False,
) -> None:
    """
    Compose and send emails with template substitution.

    Examples:

        # Interactive compose (opens neomutt)
        mail-compose -f anu --to colleague@example.com -s "Hello" -b "Body"

        # Direct send
        mail-compose -f anu --to colleague@example.com -s "Hello" -b "Body" --send

        # Reply to an existing message (auto-fills to/cc/subject)
        msg=$(mu find --fields=l "from:chris subject:Meeting" | head -1)
        mail-compose -f anu --reply-to "$msg" -b "Sounds good" --send

        # Batch with template (filter with jq before piping)
        student-db students --status confirmed | \\
            mail-compose -f phdconvenor --data - --to '{{email}}' \\
            --subject 'Hello {{preferred_name}}' --template body.md --send
    """
    config = get_account_config(account)

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

    reply_info = None
    if reply_to:
        if not reply_to.exists():
            console.print(f"[red]Reply-to message not found: {reply_to}[/red]")
            raise typer.Exit(1)
        reply_info = parse_reply_info(reply_to)
        if not to:
            to = reply_info["to"]
        if not subject:
            subject = reply_info["subject"]
        if not cc and reply_info["cc"]:
            cc = reply_info["cc"]

    if data:
        if str(data) == "-":
            data_text = sys.stdin.read()
        elif not data.exists():
            console.print(f"[red]Data file not found: {data}[/red]")
            raise typer.Exit(1)
        else:
            data_text = data.read_text()

        try:
            records = json.loads(data_text)
        except json.JSONDecodeError as e:
            console.print(f"[red]Invalid JSON: {e}[/red]")
            raise typer.Exit(1)

        if not isinstance(records, list):
            console.print("[red]JSON data must be an array[/red]")
            raise typer.Exit(1)

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
            email_to = substitute_template(to, record)
            email_subject = substitute_template(subject, record)
            email_content = substitute_template(email_body, record)
            templated_cc = substitute_template(cc, record) if cc else None
            email_cc = combine_cc(templated_cc, cc_all)

            msg = build_email(
                config.from_addr, email_to, email_subject, email_content, email_cc
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
        if not to:
            console.print("[red]--to is required[/red]")
            raise typer.Exit(1)

        if send or dry_run:
            msg = build_email(
                config.from_addr,
                to,
                subject or "",
                email_body,
                combine_cc(cc, cc_all),
                attach,
                reply_to,
            )

            success, message = send_email(msg, account, dry_run)
            if success:
                if dry_run:
                    console.print(f"{message}\n{msg.as_string()}")
                else:
                    console.print(f"Sent to {to}")
            else:
                console.print(f"[red]{message}[/red]")
                raise typer.Exit(1)
        else:
            open_neomutt_compose(
                account, to, subject or "", email_body, combine_cc(cc, cc_all), attach
            )


if __name__ == "__main__":
    app()
