#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["typer", "rich"]
# ///
"""Analyze maildir format differences and prepare messages for proper syncing.

This script examines maildir folders to identify format issues that prevent
proper mbsync synchronization, particularly for messages imported from other systems.
"""

import mailbox
import os
import re
from collections import defaultdict
from datetime import datetime
from pathlib import Path

import typer
from rich.console import Console
from rich.table import Table

console = Console()
app = typer.Typer()


def analyze_maildir(maildir_path: Path) -> dict:
    """Analyze a maildir folder's format and characteristics.
    
    Returns dict with:
    - message_count: total messages
    - filename_formats: dict of format patterns and counts
    - hostname_distribution: dict of hostnames and counts
    - timestamp_issues: messages where filename timestamp doesn't match Date header
    - uid_format: whether UIDs follow mbsync format
    - state_files: mbsync state file status
    """
    result = {
        "message_count": 0,
        "filename_formats": defaultdict(int),
        "hostname_distribution": defaultdict(int),
        "timestamp_issues": [],
        "uid_format": "unknown",
        "state_files": {},
        "sample_filenames": [],
    }
    
    # Check state files
    state_files = [".mbsyncstate", ".mbsyncstate.journal", ".mbsyncstate.new", ".uidvalidity"]
    for sf in state_files:
        sf_path = maildir_path / sf
        if sf_path.exists():
            size = sf_path.stat().st_size
            result["state_files"][sf] = f"{size:,} bytes"
    
    # Analyze messages
    try:
        mbox = mailbox.Maildir(maildir_path, create=False)
    except Exception as e:
        console.print(f"[red]Error opening maildir: {e}[/red]")
        return result
    
    keys = list(mbox.keys())
    result["message_count"] = len(keys)
    
    # Sample first 10 filenames
    for i, key in enumerate(keys[:10]):
        result["sample_filenames"].append(key)
    
    # Analyze filename patterns
    for key in keys[:1000]:  # Sample first 1000 for performance
        # Extract hostname from filename
        hostname_match = re.search(r'\.([^,:/]+),U=', key)
        if hostname_match:
            hostname = hostname_match.group(1)
            result["hostname_distribution"][hostname] += 1
        
        # Determine filename format
        if re.match(r'\d{10}\.\d+_\d+\.[^,]+,U=\d+:2,', key):
            result["filename_formats"]["old_style"] += 1
        elif re.match(r'\d{10}\.R\d+\.[^,]+,U=\d+:2,', key):
            result["filename_formats"]["mbsync_style"] += 1
            result["uid_format"] = "mbsync"
        else:
            result["filename_formats"]["other"] += 1
        
        # Check timestamp consistency (sample only)
        if len(result["timestamp_issues"]) < 10:
            try:
                # Extract timestamp from filename
                timestamp_match = re.match(r'(\d{10})', key)
                if timestamp_match:
                    file_timestamp = int(timestamp_match.group(1))
                    file_date = datetime.fromtimestamp(file_timestamp)
                    
                    # Get message Date header
                    msg = mbox[key]
                    date_header = msg.get("Date")
                    if date_header:
                        # Parse email date (simplified - doesn't handle all formats)
                        # This is just for comparison
                        year_match = re.search(r'20\d{2}', date_header)
                        if year_match:
                            email_year = int(year_match.group(0))
                            if abs(file_date.year - email_year) > 1:
                                result["timestamp_issues"].append({
                                    "key": key,
                                    "file_date": file_date.strftime("%Y-%m-%d"),
                                    "email_date": date_header,
                                })
            except Exception:
                pass
    
    # Extrapolate counts if we sampled
    if len(keys) > 1000:
        scale = len(keys) / 1000
        for fmt in result["filename_formats"]:
            result["filename_formats"][fmt] = int(result["filename_formats"][fmt] * scale)
        for host in result["hostname_distribution"]:
            result["hostname_distribution"][host] = int(result["hostname_distribution"][host] * scale)
    
    return result


@app.command()
def compare(
    maildir1: Path = typer.Argument(..., help="First maildir path to compare"),
    maildir2: Path = typer.Argument(..., help="Second maildir path to compare"),
):
    """Compare two maildir folders to identify format differences."""
    
    console.print("[bold]Analyzing maildir formats...[/bold]\n")
    
    # Analyze both directories
    analysis1 = analyze_maildir(maildir1)
    analysis2 = analyze_maildir(maildir2)
    
    # Create comparison table
    table = Table(title="Maildir Comparison")
    table.add_column("Property", style="cyan")
    table.add_column(str(maildir1.name), style="yellow")
    table.add_column(str(maildir2.name), style="green")
    
    # Message counts
    table.add_row(
        "Message Count",
        f"{analysis1['message_count']:,}",
        f"{analysis2['message_count']:,}"
    )
    
    # Filename formats
    fmt1 = ", ".join(f"{k}: {v}" for k, v in analysis1['filename_formats'].items())
    fmt2 = ", ".join(f"{k}: {v}" for k, v in analysis2['filename_formats'].items())
    table.add_row("Filename Formats", fmt1 or "none", fmt2 or "none")
    
    # Hostnames
    hosts1 = ", ".join(f"{k}: {v}" for k, v in analysis1['hostname_distribution'].items())
    hosts2 = ", ".join(f"{k}: {v}" for k, v in analysis2['hostname_distribution'].items())
    table.add_row("Hostnames", hosts1 or "none", hosts2 or "none")
    
    # State files
    state1 = ", ".join(f"{k}: {v}" for k, v in analysis1['state_files'].items())
    state2 = ", ".join(f"{k}: {v}" for k, v in analysis2['state_files'].items())
    table.add_row("State Files", state1 or "none", state2 or "none")
    
    # Timestamp issues
    table.add_row(
        "Timestamp Issues",
        f"{len(analysis1['timestamp_issues'])}",
        f"{len(analysis2['timestamp_issues'])}"
    )
    
    console.print(table)
    
    # Show sample filenames
    console.print("\n[bold]Sample filenames:[/bold]")
    console.print(f"\n{maildir1.name}:")
    for fn in analysis1['sample_filenames'][:3]:
        console.print(f"  {fn}")
    
    console.print(f"\n{maildir2.name}:")
    for fn in analysis2['sample_filenames'][:3]:
        console.print(f"  {fn}")
    
    # Show timestamp issues if any
    if analysis1['timestamp_issues']:
        console.print(f"\n[yellow]Timestamp issues in {maildir1.name}:[/yellow]")
        for issue in analysis1['timestamp_issues'][:3]:
            console.print(f"  File date: {issue['file_date']}, Email date: {issue['email_date'][:30]}...")
    
    if analysis2['timestamp_issues']:
        console.print(f"\n[yellow]Timestamp issues in {maildir2.name}:[/yellow]")
        for issue in analysis2['timestamp_issues'][:3]:
            console.print(f"  File date: {issue['file_date']}, Email date: {issue['email_date'][:30]}...")
    
    # Recommendations
    console.print("\n[bold]Sync Issues Analysis:[/bold]")
    
    if analysis1['filename_formats'].get('old_style', 0) > 0:
        console.print(f"[red]✗[/red] {maildir1.name} uses old-style filenames that may not sync properly")
    
    if analysis2['filename_formats'].get('old_style', 0) > 0:
        console.print(f"[red]✗[/red] {maildir2.name} uses old-style filenames that may not sync properly")
    
    if ".mbsyncstate.journal" in analysis1['state_files']:
        size = analysis1['state_files']['.mbsyncstate.journal']
        if "bytes" in size and int(size.split()[0].replace(",", "")) > 100000:
            console.print(f"[red]✗[/red] {maildir1.name} has large journal file ({size}) - sync state is corrupted")
    
    if ".mbsyncstate.journal" in analysis2['state_files']:
        size = analysis2['state_files']['.mbsyncstate.journal']
        if "bytes" in size and int(size.split()[0].replace(",", "")) > 100000:
            console.print(f"[red]✗[/red] {maildir2.name} has large journal file ({size}) - sync state is corrupted")
    
    if len(analysis1['timestamp_issues']) > 0:
        console.print(f"[yellow]⚠[/yellow]  {maildir1.name} has timestamp mismatches between filenames and email dates")
    
    if len(analysis2['timestamp_issues']) > 0:
        console.print(f"[yellow]⚠[/yellow]  {maildir2.name} has timestamp mismatches between filenames and email dates")


@app.command()
def prepare_for_sync(
    source_maildir: Path = typer.Argument(..., help="Source maildir to prepare for syncing"),
    dry_run: bool = typer.Option(True, "--dry-run/--execute", help="Preview changes without applying them"),
):
    """Prepare a maildir for proper mbsync synchronization.
    
    This would:
    1. Convert old-style filenames to mbsync format
    2. Fix timestamp mismatches
    3. Clean up mbsync state files
    4. Add required headers (X-TUID if missing)
    
    NOTE: This is a preview of what would be needed - actual implementation
    would require careful message file renaming and state management.
    """
    
    console.print("[bold]Analyzing maildir for sync preparation...[/bold]\n")
    
    analysis = analyze_maildir(source_maildir)
    
    console.print(f"Messages to process: {analysis['message_count']:,}")
    console.print(f"Current format: {dict(analysis['filename_formats'])}")
    
    if analysis['filename_formats'].get('old_style', 0) > 0:
        console.print(f"\n[yellow]Would need to convert {analysis['filename_formats']['old_style']:,} old-style filenames[/yellow]")
    
    if len(analysis['timestamp_issues']) > 0:
        console.print(f"\n[yellow]Would need to fix timestamps for ~{len(analysis['timestamp_issues'])} messages[/yellow]")
    
    if '.mbsyncstate.journal' in analysis['state_files']:
        console.print(f"\n[yellow]Would need to clean up corrupted sync state[/yellow]")
    
    console.print("\n[bold]Required steps to make this maildir sync-ready:[/bold]")
    console.print("1. Remove all .mbsyncstate* files to reset sync state")
    console.print("2. Convert filenames to mbsync format (timestamp.R<random>.<hostname>,U=<uid>:2,<flags>)")
    console.print("3. Use message Date header for filename timestamp (not current time)")
    console.print("4. Ensure all messages have X-TUID headers for tracking")
    console.print("5. Run initial sync with mbsync to establish proper state")
    
    if not dry_run:
        console.print("\n[red]Full implementation not yet available - this is a preview only[/red]")


if __name__ == "__main__":
    app()