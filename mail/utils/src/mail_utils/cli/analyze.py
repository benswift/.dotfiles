"""Analyze maildir format differences."""

import re
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Any

import typer
from rich.console import Console
from rich.table import Table

from mail_utils.maildir import (
    extract_hostname,
    extract_timestamp,
    is_maildir,
    is_mbsync_format,
    open_maildir,
)

console = Console()
app = typer.Typer()


def analyze_maildir(maildir_path: Path) -> dict[str, Any]:
    """Analyze a maildir folder's format and characteristics."""
    result: dict[str, Any] = {
        "message_count": 0,
        "filename_formats": defaultdict(int),
        "hostname_distribution": defaultdict(int),
        "timestamp_issues": [],
        "uid_format": "unknown",
        "state_files": {},
        "sample_filenames": [],
    }

    state_files = [
        ".mbsyncstate",
        ".mbsyncstate.journal",
        ".mbsyncstate.new",
        ".uidvalidity",
    ]
    for sf in state_files:
        sf_path = maildir_path / sf
        if sf_path.exists():
            size = sf_path.stat().st_size
            result["state_files"][sf] = f"{size:,} bytes"

    try:
        mbox = open_maildir(maildir_path)
    except Exception as e:
        console.print(f"[red]Error opening maildir: {e}[/red]")
        return result

    keys = list(mbox.keys())
    result["message_count"] = len(keys)

    for key in keys[:10]:
        result["sample_filenames"].append(key)

    for key in keys[:1000]:
        hostname = extract_hostname(key)
        if hostname:
            result["hostname_distribution"][hostname] += 1

        if is_mbsync_format(key):
            result["filename_formats"]["mbsync_style"] += 1
            result["uid_format"] = "mbsync"
        elif re.match(r"\d{10}\.\d+_\d+\.[^,]+,U=\d+:2,", key):
            result["filename_formats"]["old_style"] += 1
        else:
            result["filename_formats"]["other"] += 1

        if len(result["timestamp_issues"]) < 10:
            try:
                file_timestamp = extract_timestamp(key)
                if file_timestamp:
                    file_date = datetime.fromtimestamp(file_timestamp)
                    msg = mbox[key]
                    date_header = msg.get("Date")
                    if date_header:
                        year_match = re.search(r"20\d{2}", date_header)
                        if year_match:
                            email_year = int(year_match.group(0))
                            if abs(file_date.year - email_year) > 1:
                                result["timestamp_issues"].append(
                                    {
                                        "key": key,
                                        "file_date": file_date.strftime("%Y-%m-%d"),
                                        "email_date": date_header,
                                    }
                                )
            except Exception:
                pass

    if len(keys) > 1000:
        scale = len(keys) / 1000
        for fmt in result["filename_formats"]:
            result["filename_formats"][fmt] = int(
                result["filename_formats"][fmt] * scale
            )
        for host in result["hostname_distribution"]:
            result["hostname_distribution"][host] = int(
                result["hostname_distribution"][host] * scale
            )

    return result


@app.command()
def compare(
    maildir1: Path = typer.Argument(..., help="First maildir path to compare"),
    maildir2: Path = typer.Argument(..., help="Second maildir path to compare"),
):
    """Compare two maildir folders to identify format differences."""
    console.print("[bold]Analyzing maildir formats...[/bold]\n")

    analysis1 = analyze_maildir(maildir1)
    analysis2 = analyze_maildir(maildir2)

    table = Table(title="Maildir Comparison")
    table.add_column("Property", style="cyan")
    table.add_column(str(maildir1.name), style="yellow")
    table.add_column(str(maildir2.name), style="green")

    table.add_row(
        "Message Count",
        f"{analysis1['message_count']:,}",
        f"{analysis2['message_count']:,}",
    )

    fmt1 = ", ".join(f"{k}: {v}" for k, v in analysis1["filename_formats"].items())
    fmt2 = ", ".join(f"{k}: {v}" for k, v in analysis2["filename_formats"].items())
    table.add_row("Filename Formats", fmt1 or "none", fmt2 or "none")

    hosts1 = ", ".join(
        f"{k}: {v}" for k, v in analysis1["hostname_distribution"].items()
    )
    hosts2 = ", ".join(
        f"{k}: {v}" for k, v in analysis2["hostname_distribution"].items()
    )
    table.add_row("Hostnames", hosts1 or "none", hosts2 or "none")

    state1 = ", ".join(f"{k}: {v}" for k, v in analysis1["state_files"].items())
    state2 = ", ".join(f"{k}: {v}" for k, v in analysis2["state_files"].items())
    table.add_row("State Files", state1 or "none", state2 or "none")

    table.add_row(
        "Timestamp Issues",
        f"{len(analysis1['timestamp_issues'])}",
        f"{len(analysis2['timestamp_issues'])}",
    )

    console.print(table)

    console.print("\n[bold]Sample filenames:[/bold]")
    console.print(f"\n{maildir1.name}:")
    for fn in analysis1["sample_filenames"][:3]:
        console.print(f"  {fn}")

    console.print(f"\n{maildir2.name}:")
    for fn in analysis2["sample_filenames"][:3]:
        console.print(f"  {fn}")


@app.command()
def single(
    maildir_path: Path = typer.Argument(
        ...,
        help="Path to the maildir folder to analyze",
        exists=True,
        file_okay=False,
        dir_okay=True,
        resolve_path=True,
    ),
):
    """Analyze a single maildir folder."""
    if not is_maildir(maildir_path):
        console.print(
            f"[red]Error: {maildir_path} doesn't appear to be a maildir folder.[/red]"
        )
        raise typer.Exit(code=1)

    analysis = analyze_maildir(maildir_path)

    table = Table(title="Maildir Analysis")
    table.add_column("Property", style="cyan")
    table.add_column("Value", style="yellow")

    table.add_row("Message Count", f"{analysis['message_count']:,}")

    for fmt, count in analysis["filename_formats"].items():
        table.add_row(f"Format: {fmt}", f"{count:,}")

    for host, count in analysis["hostname_distribution"].items():
        table.add_row(f"Hostname: {host}", f"{count:,}")

    for sf, size in analysis["state_files"].items():
        table.add_row(f"State: {sf}", size)

    table.add_row("Timestamp Issues", f"{len(analysis['timestamp_issues'])}")

    console.print(table)

    if analysis["sample_filenames"]:
        console.print("\n[bold]Sample filenames:[/bold]")
        for fn in analysis["sample_filenames"][:5]:
            console.print(f"  {fn}")


def main():
    app()


if __name__ == "__main__":
    main()
