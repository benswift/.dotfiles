#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["typer", "rich", "python-dateutil"]
# ///
"""Fix maildir message filenames to use proper timestamps and mbsync-compatible format.

This script processes a maildir folder and:
1. Extracts the actual date from each message's Date header
2. Renames files to use that timestamp instead of import timestamp
3. Converts to mbsync-compatible filename format
4. Preserves all flags and message content
"""

import mailbox
import os
import random
import re
import socket
from datetime import datetime
from email.utils import parsedate_to_datetime
from pathlib import Path

import typer
from dateutil import parser as date_parser
from rich.console import Console
from rich.progress import (
    BarColumn,
    Progress,
    SpinnerColumn,
    TaskProgressColumn,
    TextColumn,
)
from rich.table import Table

console = Console()
app = typer.Typer()


def parse_email_date(date_string: str) -> datetime | None:
    """Parse an email Date header into a datetime object.
    
    Handles various date formats found in email headers.
    Returns None if parsing fails.
    """
    if not date_string:
        return None
    
    try:
        # Try standard email date parsing first
        return parsedate_to_datetime(date_string)
    except Exception:
        pass
    
    try:
        # Fall back to dateutil parser for non-standard formats
        return date_parser.parse(date_string, fuzzy=True)
    except Exception:
        pass
    
    return None


def extract_flags_from_filename(filename: str) -> str:
    """Extract the flags portion from a maildir filename.
    
    Returns the flags string (e.g., "S", "RS", "FS") or empty string.
    """
    # Look for :2, followed by flags
    match = re.search(r':2,([A-Z]*)', filename)
    if match:
        return match.group(1)
    return ""


def extract_uid_from_filename(filename: str) -> str | None:
    """Extract the UID from a maildir filename.
    
    Returns the UID number or None if not found.
    """
    match = re.search(r',U=(\d+)', filename)
    if match:
        return match.group(1)
    return None


def generate_mbsync_filename(
    timestamp: int,
    hostname: str,
    uid: str | None,
    flags: str,
    use_random_id: bool = True
) -> str:
    """Generate an mbsync-compatible maildir filename.
    
    Format: timestamp.R<random_id>.hostname,U=<uid>:2,<flags>
    or: timestamp.<seq>_<pid>.hostname,U=<uid>:2,<flags> (if not using random ID)
    """
    if use_random_id:
        # Generate a large random ID like mbsync does
        random_id = random.randint(10**15, 10**18 - 1)
        base = f"{timestamp}.R{random_id}.{hostname}"
    else:
        # Use traditional format with PID and sequence
        pid = os.getpid()
        seq = random.randint(1, 100000)
        base = f"{timestamp}.{pid}_{seq}.{hostname}"
    
    # Add UID if present
    if uid:
        base = f"{base},U={uid}"
    
    # Add flags
    base = f"{base}:2,{flags}"
    
    return base


def analyze_maildir(maildir_path: Path) -> dict:
    """Analyze a maildir to identify issues and gather statistics.
    
    Returns a dict with analysis results.
    """
    stats = {
        "total_messages": 0,
        "timestamp_mismatches": 0,
        "missing_dates": 0,
        "old_format": 0,
        "mbsync_format": 0,
        "sample_issues": [],
    }
    
    # Check for messages in cur and new directories
    cur_dir = maildir_path / "cur"
    new_dir = maildir_path / "new"
    
    all_files = []
    if cur_dir.exists():
        all_files.extend([(cur_dir, f.name) for f in cur_dir.iterdir() if f.is_file()])
    if new_dir.exists():
        all_files.extend([(new_dir, f.name) for f in new_dir.iterdir() if f.is_file()])
    
    stats["total_messages"] = len(all_files)
    
    if stats["total_messages"] == 0:
        return stats
    
    # Open maildir for reading headers
    try:
        mbox = mailbox.Maildir(maildir_path, create=False)
    except Exception as e:
        console.print(f"[red]Error opening maildir: {e}[/red]")
        return stats
    
    # Sample analysis (first 100 messages for performance)
    sample_size = min(100, len(all_files))
    
    for directory, filename in all_files[:sample_size]:
        # Check filename format
        if re.match(r'\d{10}\.R\d+\.[^,]+,U=\d+:2,', filename):
            stats["mbsync_format"] += 1
        else:
            stats["old_format"] += 1
        
        # Extract timestamp from filename
        timestamp_match = re.match(r'(\d{10})', filename)
        if not timestamp_match:
            continue
            
        file_timestamp = int(timestamp_match.group(1))
        file_date = datetime.fromtimestamp(file_timestamp)
        
        # Try to get the message date
        try:
            # Find the message key that corresponds to this file
            for key in mbox.keys():
                if filename in key:
                    msg = mbox[key]
                    date_header = msg.get("Date")
                    if date_header:
                        email_date = parse_email_date(date_header)
                        if email_date:
                            # Check if timestamps differ by more than a day
                            diff = abs((file_date - email_date).total_seconds())
                            if diff > 86400:  # More than 1 day difference
                                stats["timestamp_mismatches"] += 1
                                if len(stats["sample_issues"]) < 5:
                                    stats["sample_issues"].append({
                                        "filename": filename[:50] + "...",
                                        "file_date": file_date.strftime("%Y-%m-%d"),
                                        "email_date": email_date.strftime("%Y-%m-%d"),
                                    })
                    else:
                        stats["missing_dates"] += 1
                    break
        except Exception:
            pass
    
    # Extrapolate if we sampled
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
    """Fix timestamps and convert filenames to mbsync format.
    
    Args:
        maildir_path: Path to the maildir folder
        dry_run: If True, only preview changes without applying them
        verbose: If True, show detailed information
        hostname: Hostname to use in new filenames (default: current hostname)
        use_random_id: If True, use mbsync-style random IDs
    
    Returns:
        Tuple of (total processed, successful, failed)
    """
    if hostname is None:
        hostname = socket.gethostname().split('.')[0]
    
    console.print(f"\n[bold]Processing maildir: {maildir_path}[/bold]")
    console.print(f"Using hostname: {hostname}")
    
    # Get all message files
    cur_dir = maildir_path / "cur"
    new_dir = maildir_path / "new"
    
    all_files = []
    if cur_dir.exists():
        all_files.extend([(cur_dir, f) for f in cur_dir.iterdir() if f.is_file()])
    if new_dir.exists():
        all_files.extend([(new_dir, f) for f in new_dir.iterdir() if f.is_file()])
    
    if not all_files:
        console.print("[yellow]No messages found in maildir[/yellow]")
        return 0, 0, 0
    
    console.print(f"Found {len(all_files):,} messages to process")
    
    # Open maildir for reading headers
    try:
        mbox = mailbox.Maildir(maildir_path, create=False)
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
            
            # Extract current components
            flags = extract_flags_from_filename(filename)
            uid = extract_uid_from_filename(filename)
            
            # Find the message in the mailbox
            # The mailbox module strips :2,FLAGS from keys, so we need to match the base part
            message_date = None
            base_filename = filename.split(':2,')[0]
            
            for key in mbox.keys():
                if base_filename in key or key in filename:
                    try:
                        msg = mbox[key]
                        date_header = msg.get("Date")
                        if date_header:
                            message_date = parse_email_date(date_header)
                    except Exception as e:
                        if verbose:
                            console.print(f"[yellow]Could not read message {filename}: {e}[/yellow]")
                    break
            
            if not message_date:
                # Try to extract from filename if no Date header
                timestamp_match = re.match(r'(\d{10})', filename)
                if timestamp_match:
                    message_date = datetime.fromtimestamp(int(timestamp_match.group(1)))
                else:
                    failed += 1
                    if verbose:
                        console.print(f"[red]No date found for {filename}[/red]")
                    progress.update(task, advance=1)
                    continue
            
            # Generate new filename
            timestamp = int(message_date.timestamp())
            new_filename = generate_mbsync_filename(
                timestamp, hostname, uid, flags, use_random_id
            )
            
            # Check if rename is needed
            if filename != new_filename:
                old_path = file_path
                new_path = directory / new_filename
                
                # Check for conflicts
                if new_path.exists():
                    # Add a small random component to avoid conflicts
                    timestamp += random.randint(0, 10)
                    new_filename = generate_mbsync_filename(
                        timestamp, hostname, uid, flags, use_random_id
                    )
                    new_path = directory / new_filename
                
                renames.append((old_path, new_path, filename, new_filename))
                successful += 1
                
                if verbose and len(renames) <= 10:
                    console.print(f"[cyan]{filename}[/cyan]")
                    console.print(f"  → [green]{new_filename}[/green]")
            
            progress.update(task, advance=1)
    
    # Show summary
    console.print(f"\n[bold]Summary:[/bold]")
    console.print(f"  Total messages: {processed:,}")
    console.print(f"  Messages to rename: {successful:,}")
    console.print(f"  Failed to process: {failed:,}")
    console.print(f"  Already correct: {processed - successful - failed:,}")
    
    if not renames:
        console.print("\n[green]No changes needed![/green]")
        return processed, successful, failed
    
    # Show sample renames
    if renames and not verbose:
        console.print("\n[bold]Sample renames:[/bold]")
        for old_path, new_path, old_name, new_name in renames[:5]:
            console.print(f"[cyan]{old_name}[/cyan]")
            console.print(f"  → [green]{new_name}[/green]")
        if len(renames) > 5:
            console.print(f"  ... and {len(renames) - 5:,} more")
    
    if dry_run:
        console.print("\n[yellow]DRY RUN: No files were renamed[/yellow]")
        console.print("Run with --execute to apply changes")
        return processed, successful, failed
    
    # Apply renames
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
    
    # Verify maildir structure
    has_maildir_structure = any(
        (maildir_path / subdir).exists() for subdir in ["cur", "new", "tmp"]
    )
    
    if not has_maildir_structure:
        console.print(
            f"[red]Error: {maildir_path} doesn't appear to be a maildir folder.[/red]"
        )
        raise typer.Exit(code=1)
    
    stats = analyze_maildir(maildir_path)
    
    # Display results
    table = Table(title="Maildir Analysis")
    table.add_column("Metric", style="cyan")
    table.add_column("Value", style="yellow")
    
    table.add_row("Total messages", f"{stats['total_messages']:,}")
    table.add_row("Old format filenames", f"{stats['old_format']:,}")
    table.add_row("mbsync format filenames", f"{stats['mbsync_format']:,}")
    table.add_row("Timestamp mismatches", f"{stats['timestamp_mismatches']:,}")
    table.add_row("Missing Date headers", f"{stats['missing_dates']:,}")
    
    console.print(table)
    
    if stats['sample_issues']:
        console.print("\n[bold]Sample timestamp issues:[/bold]")
        for issue in stats['sample_issues']:
            console.print(f"  {issue['filename']}")
            console.print(f"    File date: {issue['file_date']}, Email date: {issue['email_date']}")
    
    if stats['timestamp_mismatches'] > 0 or stats['old_format'] > 0:
        console.print("\n[yellow]This maildir needs fixing. Run with 'fix' command.[/yellow]")


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
        "-h",
        help="Hostname to use in new filenames (default: current hostname)",
    ),
    random_id: bool = typer.Option(
        True,
        "--random-id/--sequential-id",
        help="Use mbsync-style random IDs (recommended)",
    ),
):
    """Fix maildir timestamps and convert to mbsync-compatible format.
    
    This command will:
    1. Extract the actual date from each message's Date header
    2. Rename files to use that timestamp
    3. Convert filenames to mbsync-compatible format
    4. Preserve all flags and UIDs
    """
    
    # Verify maildir structure
    has_maildir_structure = any(
        (maildir_path / subdir).exists() for subdir in ["cur", "new", "tmp"]
    )
    
    if not has_maildir_structure:
        console.print(
            f"[red]Error: {maildir_path} doesn't appear to be a maildir folder.[/red]"
        )
        raise typer.Exit(code=1)
    
    # Check for mbsync state files
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
            console.print("\n[yellow]Remember to remove mbsync state files before syncing:[/yellow]")
            for f in existing_state:
                console.print(f"  rm {maildir_path / f}")
        
    except KeyboardInterrupt:
        console.print("\n[yellow]Interrupted by user[/yellow]")
        raise typer.Exit(code=130)
    except Exception as e:
        console.print(f"[red]Error: {e}[/red]")
        raise typer.Exit(code=1)


if __name__ == "__main__":
    app()