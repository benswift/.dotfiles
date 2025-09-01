#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["typer", "rich"]
# ///
"""Deduplicate email messages in a maildir folder based on Message-ID headers.

This script uses Python's mailbox module to scan a maildir folder,
identifies duplicate emails using Message-ID headers, keeps the first occurrence
(by key sort), and deletes subsequent duplicates.
"""

import mailbox
from collections import defaultdict
from pathlib import Path

import typer
from rich.console import Console
from rich.progress import (
    BarColumn,
    Progress,
    SpinnerColumn,
    TaskProgressColumn,
    TextColumn,
)

console = Console()
app = typer.Typer()


def find_duplicates(
    mbox: mailbox.Maildir, show_progress: bool = True
) -> tuple[dict[str, list[str]], int]:
    """Find duplicate messages in a maildir folder.

    Args:
        mbox: Maildir object
        show_progress: Whether to show progress bar

    Returns:
        A tuple of (dict mapping Message-IDs to lists of message keys, total message count)
    """
    message_id_to_keys: dict[str, list[str]] = defaultdict(list)
    
    # Get all message keys
    keys = list(mbox.keys())
    total_messages = len(keys)

    if total_messages == 0:
        return message_id_to_keys, 0

    if show_progress:
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            BarColumn(),
            TaskProgressColumn(),
            console=console,
        ) as progress:
            task = progress.add_task(
                f"Scanning {total_messages} messages...", total=total_messages
            )

            for key in keys:
                try:
                    msg = mbox[key]
                    msg_id = msg.get("Message-ID")
                    if msg_id:
                        message_id_to_keys[msg_id.strip()].append(key)
                except Exception as e:
                    console.print(f"[yellow]Warning: Could not read message {key}: {e}[/yellow]")
                progress.update(task, advance=1)
    else:
        for key in keys:
            try:
                msg = mbox[key]
                msg_id = msg.get("Message-ID")
                if msg_id:
                    message_id_to_keys[msg_id.strip()].append(key)
            except Exception as e:
                console.print(f"[yellow]Warning: Could not read message {key}: {e}[/yellow]")

    return message_id_to_keys, total_messages


def deduplicate(
    maildir_path: Path, dry_run: bool = False, verbose: bool = False
) -> tuple[int, int, int]:
    """Deduplicate messages in a maildir folder.

    Args:
        maildir_path: Path to the maildir folder
        dry_run: If True, only preview what would be deleted
        verbose: If True, print detailed information

    Returns:
        A tuple of (total messages processed, unique messages kept, duplicates removed)
    """
    console.print(f"\n[bold]Scanning maildir: {maildir_path}[/bold]")

    try:
        # Open the maildir
        mbox = mailbox.Maildir(maildir_path, create=False)
    except Exception as e:
        console.print(f"[red]Error opening maildir: {e}[/red]")
        return 0, 0, 0

    # Find all messages and group by Message-ID
    message_id_to_keys, total_messages = find_duplicates(mbox)

    if total_messages == 0:
        console.print("[yellow]No messages found in maildir[/yellow]")
        return 0, 0, 0

    # Count duplicates
    unique_count = len(message_id_to_keys)
    duplicate_count = 0
    keys_to_delete = []

    for message_id, keys in message_id_to_keys.items():
        if len(keys) > 1:
            # Sort keys to ensure consistent behavior
            sorted_keys = sorted(keys)
            # Keep the first one, mark the rest for deletion
            keys_to_delete.extend(sorted_keys[1:])
            duplicate_count += len(sorted_keys) - 1

            if verbose and len(sorted_keys) > 1:
                console.print(f"\n[cyan]Message-ID: {message_id}[/cyan]")
                console.print(f"  [green]Keeping:[/green] {sorted_keys[0]}")
                for key in sorted_keys[1:]:
                    console.print(f"  [red]Deleting:[/red] {key}")

    # Print summary
    console.print("\n[bold]Summary:[/bold]")
    console.print(f"  Total messages scanned: {total_messages:,}")
    console.print(f"  Unique messages: {unique_count:,}")
    console.print(f"  Duplicate messages to remove: {duplicate_count:,}")

    if duplicate_count == 0:
        console.print("\n[green]No duplicates found![/green]")
        return total_messages, unique_count, 0

    if dry_run:
        console.print("\n[yellow]DRY RUN: No messages were deleted[/yellow]")
        return total_messages, unique_count, duplicate_count

    # Delete duplicates
    console.print(f"\n[bold]Deleting {duplicate_count:,} duplicate messages...[/bold]")

    deleted_count = 0
    failed_count = 0

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        TaskProgressColumn(),
        console=console,
    ) as progress:
        task = progress.add_task("Deleting duplicates...", total=len(keys_to_delete))

        for key in keys_to_delete:
            try:
                mbox.remove(key)
                deleted_count += 1
            except Exception as e:
                console.print(f"[red]Failed to delete message {key}: {e}[/red]")
                failed_count += 1
            progress.update(task, advance=1)

    # Flush changes to disk
    mbox.flush()
    mbox.close()

    console.print(
        f"\n[green]Successfully deleted {deleted_count:,} duplicate messages[/green]"
    )
    if failed_count > 0:
        console.print(f"[red]Failed to delete {failed_count:,} messages[/red]")

    return total_messages, unique_count, deleted_count


@app.command()
def main(
    maildir_path: Path = typer.Argument(
        ...,
        help="Path to the maildir folder to deduplicate",
        exists=True,
        file_okay=False,
        dir_okay=True,
        resolve_path=True,
    ),
    dry_run: bool = typer.Option(
        False,
        "--dry-run",
        "-n",
        help="Preview what would be deleted without actually deleting",
    ),
    verbose: bool = typer.Option(
        False, "--verbose", "-v", help="Show detailed information about duplicates"
    ),
):
    """Deduplicate email messages in a maildir folder based on Message-ID headers.

    This script uses Python's mailbox module to scan a maildir folder,
    identifies duplicate emails using Message-ID headers, keeps the first occurrence
    (by key sort), and deletes subsequent duplicates.
    """
    try:
        # Verify it looks like a maildir
        has_maildir_structure = any(
            (maildir_path / subdir).exists() for subdir in ["cur", "new", "tmp"]
        )

        if not has_maildir_structure:
            console.print(
                f"[red]Error: {maildir_path} doesn't appear to be a maildir folder.[/red]\n"
                "Expected to find at least one of: cur/, new/, tmp/ subdirectories"
            )
            raise typer.Exit(code=1)

        # Run deduplication
        total, unique, removed = deduplicate(maildir_path, dry_run, verbose)

        # Exit with appropriate code
        if total == 0:
            raise typer.Exit(code=1)

    except KeyboardInterrupt:
        console.print("\n[yellow]Interrupted by user[/yellow]")
        raise typer.Exit(code=130)
    except Exception as e:
        console.print(f"[red]Error: {e}[/red]")
        raise typer.Exit(code=1)


if __name__ == "__main__":
    app()
