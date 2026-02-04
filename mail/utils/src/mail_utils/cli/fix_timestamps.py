"""Fix maildir message filenames to use proper timestamps and mbsync-compatible format."""

import random
import socket
from datetime import datetime
from pathlib import Path
from typing import Any

import typer
from rich.console import Console
from rich.progress import (
    BarColumn,
    Progress,
    SpinnerColumn,
    TaskProgressColumn,
    TextColumn,
)
from rich.table import Table

from mail_utils.email import parse_email_date
from mail_utils.maildir import (
    extract_flags,
    extract_timestamp,
    extract_uid,
    generate_filename,
    get_all_message_files,
    is_maildir,
    is_mbsync_format,
    open_maildir,
)

console = Console()
app = typer.Typer()


def analyze_for_fixes(maildir_path: Path) -> dict[str, Any]:
    """Analyze a maildir to identify issues and gather statistics."""
    stats: dict[str, Any] = {
        "total_messages": 0,
        "timestamp_mismatches": 0,
        "missing_dates": 0,
        "old_format": 0,
        "mbsync_format": 0,
        "sample_issues": [],
    }

    all_files = get_all_message_files(maildir_path)
    stats["total_messages"] = len(all_files)

    if stats["total_messages"] == 0:
        return stats

    try:
        mbox = open_maildir(maildir_path)
    except Exception as e:
        console.print(f"[red]Error opening maildir: {e}[/red]")
        return stats

    sample_size = min(100, len(all_files))

    for directory, file_path in all_files[:sample_size]:
        filename = file_path.name

        if is_mbsync_format(filename):
            stats["mbsync_format"] += 1
        else:
            stats["old_format"] += 1

        file_timestamp = extract_timestamp(filename)
        if not file_timestamp:
            continue

        file_date = datetime.fromtimestamp(file_timestamp)

        try:
            for key in mbox.keys():
                if filename in key:
                    msg = mbox[key]
                    date_header = msg.get("Date")
                    if date_header:
                        email_date = parse_email_date(date_header)
                        if email_date:
                            diff = abs((file_date - email_date).total_seconds())
                            if diff > 86400:
                                stats["timestamp_mismatches"] += 1
                                if len(stats["sample_issues"]) < 5:
                                    stats["sample_issues"].append(
                                        {
                                            "filename": filename[:50] + "...",
                                            "file_date": file_date.strftime("%Y-%m-%d"),
                                            "email_date": email_date.strftime(
                                                "%Y-%m-%d"
                                            ),
                                        }
                                    )
                    else:
                        stats["missing_dates"] += 1
                    break
        except Exception:
            pass

    if sample_size < stats["total_messages"]:
        scale = stats["total_messages"] / sample_size
        stats["timestamp_mismatches"] = int(stats["timestamp_mismatches"] * scale)
        stats["missing_dates"] = int(stats["missing_dates"] * scale)
        stats["old_format"] = int(stats["old_format"] * scale)
        stats["mbsync_format"] = int(stats["mbsync_format"] * scale)

    return stats


def fix_maildir_timestamps(
    maildir_path: Path,
    dry_run: bool = True,
    verbose: bool = False,
    hostname: str | None = None,
    use_random_id: bool = True,
) -> tuple[int, int, int]:
    """Fix timestamps and convert filenames to mbsync format."""
    if hostname is None:
        hostname = socket.gethostname().split(".")[0]

    console.print(f"\n[bold]Processing maildir: {maildir_path}[/bold]")
    console.print(f"Using hostname: {hostname}")

    all_files = get_all_message_files(maildir_path)

    if not all_files:
        console.print("[yellow]No messages found in maildir[/yellow]")
        return 0, 0, 0

    console.print(f"Found {len(all_files):,} messages to process")

    try:
        mbox = open_maildir(maildir_path)
    except Exception as e:
        console.print(f"[red]Error opening maildir: {e}[/red]")
        return 0, 0, 0

    processed = 0
    successful = 0
    failed = 0
    renames = []

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        TaskProgressColumn(),
        console=console,
    ) as progress:
        task = progress.add_task("Analyzing messages...", total=len(all_files))

        for directory, file_path in all_files:
            processed += 1
            filename = file_path.name

            flags = extract_flags(filename)
            uid = extract_uid(filename)

            message_date = None
            base_filename = filename.split(":2,")[0]

            for key in mbox.keys():
                if base_filename in key or key in filename:
                    try:
                        msg = mbox[key]
                        date_header = msg.get("Date")
                        if date_header:
                            message_date = parse_email_date(date_header)
                    except Exception as e:
                        if verbose:
                            console.print(
                                f"[yellow]Could not read message {filename}: {e}[/yellow]"
                            )
                    break

            if not message_date:
                file_timestamp = extract_timestamp(filename)
                if file_timestamp:
                    message_date = datetime.fromtimestamp(file_timestamp)
                else:
                    failed += 1
                    if verbose:
                        console.print(f"[red]No date found for {filename}[/red]")
                    progress.update(task, advance=1)
                    continue

            timestamp = int(message_date.timestamp())
            new_filename = generate_filename(
                timestamp, hostname, uid, flags, use_random_id
            )

            if filename != new_filename:
                old_path = file_path
                new_path = directory / new_filename

                if new_path.exists():
                    timestamp += random.randint(0, 10)
                    new_filename = generate_filename(
                        timestamp, hostname, uid, flags, use_random_id
                    )
                    new_path = directory / new_filename

                renames.append((old_path, new_path, filename, new_filename))
                successful += 1

                if verbose and len(renames) <= 10:
                    console.print(f"[cyan]{filename}[/cyan]")
                    console.print(f"  -> [green]{new_filename}[/green]")

            progress.update(task, advance=1)

    console.print("\n[bold]Summary:[/bold]")
    console.print(f"  Total messages: {processed:,}")
    console.print(f"  Messages to rename: {successful:,}")
    console.print(f"  Failed to process: {failed:,}")
    console.print(f"  Already correct: {processed - successful - failed:,}")

    if not renames:
        console.print("\n[green]No changes needed![/green]")
        return processed, successful, failed

    if renames and not verbose:
        console.print("\n[bold]Sample renames:[/bold]")
        for old_path, new_path, old_name, new_name in renames[:5]:
            console.print(f"[cyan]{old_name}[/cyan]")
            console.print(f"  -> [green]{new_name}[/green]")
        if len(renames) > 5:
            console.print(f"  ... and {len(renames) - 5:,} more")

    if dry_run:
        console.print("\n[yellow]DRY RUN: No files were renamed[/yellow]")
        console.print("Run with --execute to apply changes")
        return processed, successful, failed

    console.print(f"\n[bold]Renaming {len(renames):,} files...[/bold]")

    rename_success = 0
    rename_failed = 0

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        TaskProgressColumn(),
        console=console,
    ) as progress:
        task = progress.add_task("Renaming files...", total=len(renames))

        for old_path, new_path, old_name, new_name in renames:
            try:
                old_path.rename(new_path)
                rename_success += 1
            except Exception as e:
                rename_failed += 1
                if verbose:
                    console.print(f"[red]Failed to rename {old_name}: {e}[/red]")
            progress.update(task, advance=1)

    console.print(f"\n[green]Successfully renamed {rename_success:,} files[/green]")
    if rename_failed > 0:
        console.print(f"[red]Failed to rename {rename_failed:,} files[/red]")

    return processed, rename_success, rename_failed


@app.command()
def analyze(
    maildir_path: Path = typer.Argument(
        ...,
        help="Path to the maildir folder to analyze",
        exists=True,
        file_okay=False,
        dir_okay=True,
        resolve_path=True,
    ),
):
    """Analyze a maildir folder for timestamp and format issues."""
    if not is_maildir(maildir_path):
        console.print(
            f"[red]Error: {maildir_path} doesn't appear to be a maildir folder.[/red]"
        )
        raise typer.Exit(code=1)

    stats = analyze_for_fixes(maildir_path)

    table = Table(title="Maildir Analysis")
    table.add_column("Metric", style="cyan")
    table.add_column("Value", style="yellow")

    table.add_row("Total messages", f"{stats['total_messages']:,}")
    table.add_row("Old format filenames", f"{stats['old_format']:,}")
    table.add_row("mbsync format filenames", f"{stats['mbsync_format']:,}")
    table.add_row("Timestamp mismatches", f"{stats['timestamp_mismatches']:,}")
    table.add_row("Missing Date headers", f"{stats['missing_dates']:,}")

    console.print(table)

    if stats["sample_issues"]:
        console.print("\n[bold]Sample timestamp issues:[/bold]")
        for issue in stats["sample_issues"]:
            console.print(f"  {issue['filename']}")
            console.print(
                f"    File date: {issue['file_date']}, Email date: {issue['email_date']}"
            )

    if stats["timestamp_mismatches"] > 0 or stats["old_format"] > 0:
        console.print(
            "\n[yellow]This maildir needs fixing. Run 'mail-fix-timestamps fix' command.[/yellow]"
        )


@app.command()
def fix(
    maildir_path: Path = typer.Argument(
        ...,
        help="Path to the maildir folder to fix",
        exists=True,
        file_okay=False,
        dir_okay=True,
        resolve_path=True,
    ),
    dry_run: bool = typer.Option(
        True,
        "--dry-run/--execute",
        help="Preview changes without applying them",
    ),
    verbose: bool = typer.Option(
        False,
        "--verbose",
        "-v",
        help="Show detailed information about each rename",
    ),
    hostname: str = typer.Option(
        None,
        "--hostname",
        help="Hostname to use in new filenames (default: current hostname)",
    ),
    random_id: bool = typer.Option(
        True,
        "--random-id/--sequential-id",
        help="Use mbsync-style random IDs (recommended)",
    ),
):
    """Fix maildir timestamps and convert to mbsync-compatible format."""
    if not is_maildir(maildir_path):
        console.print(
            f"[red]Error: {maildir_path} doesn't appear to be a maildir folder.[/red]"
        )
        raise typer.Exit(code=1)

    state_files = [".mbsyncstate", ".mbsyncstate.journal", ".mbsyncstate.new"]
    existing_state = [f for f in state_files if (maildir_path / f).exists()]

    if existing_state and not dry_run:
        console.print("[yellow]Warning: Found mbsync state files:[/yellow]")
        for f in existing_state:
            size = (maildir_path / f).stat().st_size
            console.print(f"  {f} ({size:,} bytes)")
        console.print("\nThese should be removed before syncing the fixed maildir.")

        if not typer.confirm("Continue anyway?"):
            raise typer.Exit(code=0)

    try:
        total, successful, failed = fix_maildir_timestamps(
            maildir_path, dry_run, verbose, hostname, random_id
        )

        if not dry_run and existing_state:
            console.print(
                "\n[yellow]Remember to remove mbsync state files before syncing:[/yellow]"
            )
            for f in existing_state:
                console.print(f"  rm {maildir_path / f}")

    except KeyboardInterrupt:
        console.print("\n[yellow]Interrupted by user[/yellow]")
        raise typer.Exit(code=130)


def main():
    app()


if __name__ == "__main__":
    main()
