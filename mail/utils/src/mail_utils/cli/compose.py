"""Compose and send emails with template substitution support."""

import json
import re
import sys
from pathlib import Path
from typing import Annotated, Optional

import typer
from jinja2 import Environment, StrictUndefined
from rich.console import Console

from mail_utils.accounts import Account, get_account_config
from mail_utils.compose import (
    build_email,
    combine_cc,
    open_neomutt_compose,
    send_email,
    strip_frontmatter,
)

console = Console()
app = typer.Typer(
    help="Compose and send emails with template substitution.",
    add_completion=False,
    no_args_is_help=True,
)


def parse_json_lenient(text: str) -> list[dict]:
    """Parse JSON with trailing commas allowed."""
    cleaned = re.sub(r",(\s*[}\]])", r"\1", text)
    result = json.loads(cleaned)
    if not isinstance(result, list):
        raise ValueError("JSON must be an array")
    return result


JINJA_ENV = Environment(undefined=StrictUndefined)


def substitute_template(template: str, data: dict) -> str:
    """Render a Jinja2 template with data."""
    return JINJA_ENV.from_string(template).render(data)


def filter_records(records: list[dict], filter_expr: str | None) -> list[dict]:
    """Filter records using a Python expression."""
    if not filter_expr:
        return records

    results = []
    for record in records:
        try:
            if eval(filter_expr, {"__builtins__": {}}, record):
                results.append(record)
        except Exception:
            pass
    return results


@app.command()
def main(
    account: Annotated[
        Account, typer.Option("--from", "-f", help="Account to send from")
    ],
    to: Annotated[
        Optional[str],
        typer.Option(help="Recipient (supports {{field}} templates)"),
    ] = None,
    cc: Annotated[
        Optional[str],
        typer.Option(
            "--cc", "-c", help="CC recipient (supports templates in batch mode)"
        ),
    ] = None,
    cc_all: Annotated[
        Optional[str], typer.Option("--cc-all", help="CC recipient added to all emails")
    ] = None,
    subject: Annotated[
        Optional[str], typer.Option("--subject", "-s", help="Subject line")
    ] = None,
    body: Annotated[
        Optional[str],
        typer.Option("--body", "-b", help="Body text (or '-' for stdin)"),
    ] = None,
    template: Annotated[
        Optional[Path], typer.Option(help="Template file for body")
    ] = None,
    attach: Annotated[
        Optional[list[Path]], typer.Option("--attach", "-a", help="Attachment")
    ] = None,
    data: Annotated[
        Optional[Path],
        typer.Option(help="JSON file for batch mode (use '-' for stdin)"),
    ] = None,
    filter_expr: Annotated[
        Optional[str], typer.Option("--filter", help="Filter expression")
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

        # Batch with template
        mail-compose -f phdconvenor --data students.json --to 'u{{id}}@anu.edu.au' \\
            --subject 'Hello {{preferred_name}}' --template body.md \\
            --filter 'status == "active"' --send
    """
    config = get_account_config(account)

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

    if data:
        if str(data) == "-":
            data_text = sys.stdin.read()
        elif not data.exists():
            console.print(f"[red]Data file not found: {data}[/red]")
            raise typer.Exit(1)
        else:
            data_text = data.read_text()

        try:
            records = parse_json_lenient(data_text)
        except ValueError as e:
            console.print(f"[red]Invalid data file: {e}[/red]")
            raise typer.Exit(1)

        records = filter_records(records, filter_expr)
        if filter_expr:
            console.print(f"Filtered to {len(records)} record(s)")

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
