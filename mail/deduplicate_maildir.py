#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["typer", "rich"]
# ///
"""Deduplicate email messages in a maildir folder based on Message-ID headers.

This script scans a maildir folder structure (with cur/, new/, tmp/ subdirectories),
identifies duplicate emails using Message-ID headers, keeps the first occurrence
(by filename sort), and deletes subsequent duplicates.
"""

import email
import email.parser
from collections import defaultdict
from pathlib import Path
from typing import Iterator

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


def extract_message_id(file_path: Path) -> str | None:
    """Extract Message-ID from an email file.

    Args:
        file_path: Path to the email file

    Returns:
        The Message-ID if found, None otherwise
    """
    try:
        with open(file_path, "rb") as f:
            # Use email parser that handles multi-line headers correctly
            parser = email.parser.BytesHeaderParser()
            msg = parser.parse(f)

            # Get the Message-ID header (parser handles multi-line headers)
            msg_id = msg.get("Message-ID")
            if msg_id:
                # The parser returns the full value with whitespace normalized
                return msg_id.strip()

    except Exception as e:
        console.print(f"[yellow]Warning: Could not read {file_path}: {e}[/yellow]")
    return None


def scan_maildir(maildir_path: Path) -> Iterator[Path]:
    """Scan a maildir folder for email files.

    Args:
        maildir_path: Path to the maildir folder

    Yields:
        Paths to email files
    """
    # Maildir folders typically have cur/, new/, and tmp/ subdirectories
    subdirs = ["cur", "new", "tmp"]

    for subdir in subdirs:
        subdir_path = maildir_path / subdir
        if subdir_path.exists() and subdir_path.is_dir():
            # Yield all files (excluding . files and directories)
            for file_path in subdir_path.iterdir():
                if file_path.is_file() and not file_path.name.startswith("."):
                    yield file_path


def find_duplicates(
    maildir_path: Path, show_progress: bool = True
) -> tuple[dict[str, list[Path]], int]:
    """Find duplicate messages in a maildir folder.

    Args:
        maildir_path: Path to the maildir folder
        show_progress: Whether to show progress bar

    Returns:
        A tuple of (dict mapping Message-IDs to lists of file paths, total file count)
    """
    message_id_to_files: dict[str, list[Path]] = defaultdict(list)
    total_files = 0

    # First, count total files for progress bar
    all_files = list(scan_maildir(maildir_path))
    total_files = len(all_files)

    if total_files == 0:
        return message_id_to_files, 0

    if show_progress:
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            BarColumn(),
            TaskProgressColumn(),
            console=console,
        ) as progress:
            task = progress.add_task(
                f"Scanning {total_files} files...", total=total_files
            )

            for file_path in all_files:
                message_id = extract_message_id(file_path)
                if message_id:
                    message_id_to_files[message_id].append(file_path)
                progress.update(task, advance=1)
    else:
        for file_path in all_files:
            message_id = extract_message_id(file_path)
            if message_id:
                message_id_to_files[message_id].append(file_path)

    return message_id_to_files, total_files


def deduplicate(
    maildir_path: Path, dry_run: bool = False, verbose: bool = False
) -> tuple[int, int, int]:
    """Deduplicate messages in a maildir folder.

    Args:
        maildir_path: Path to the maildir folder
        dry_run: If True, only preview what would be deleted
        verbose: If True, print detailed information

    Returns:
        A tuple of (total files processed, unique messages kept, duplicates removed)
    """
    console.print(f"\n[bold]Scanning maildir: {maildir_path}[/bold]")

    # Find all messages and group by Message-ID
    message_id_to_files, total_files = find_duplicates(maildir_path)

    if total_files == 0:
        console.print("[yellow]No email files found in maildir[/yellow]")
        return 0, 0, 0

    # Count duplicates
    unique_count = len(message_id_to_files)
    duplicate_count = 0
    files_to_delete = []

    for message_id, file_paths in message_id_to_files.items():
        if len(file_paths) > 1:
            # Sort files by name to ensure consistent behavior
            sorted_paths = sorted(file_paths, key=lambda p: str(p))
            # Keep the first one, mark the rest for deletion
            files_to_delete.extend(sorted_paths[1:])
            duplicate_count += len(sorted_paths) - 1

            if verbose and len(sorted_paths) > 1:
                console.print(f"\n[cyan]Message-ID: {message_id}[/cyan]")
                console.print(f"  [green]Keeping:[/green] {sorted_paths[0].name}")
                for path in sorted_paths[1:]:
                    console.print(f"  [red]Deleting:[/red] {path.name}")

    # Print summary
    console.print("\n[bold]Summary:[/bold]")
    console.print(f"  Total files scanned: {total_files:,}")
    console.print(f"  Unique messages: {unique_count:,}")
    console.print(f"  Duplicate files to remove: {duplicate_count:,}")

    if duplicate_count == 0:
        console.print("\n[green]No duplicates found![/green]")
        return total_files, unique_count, 0

    if dry_run:
        console.print("\n[yellow]DRY RUN: No files were deleted[/yellow]")
        return total_files, unique_count, duplicate_count

    # Delete duplicates
    console.print(f"\n[bold]Deleting {duplicate_count:,} duplicate files...[/bold]")

    deleted_count = 0
    failed_count = 0

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        TaskProgressColumn(),
        console=console,
    ) as progress:
        task = progress.add_task("Deleting duplicates...", total=len(files_to_delete))

        for file_path in files_to_delete:
            try:
                file_path.unlink()
                deleted_count += 1
            except Exception as e:
                console.print(f"[red]Failed to delete {file_path}: {e}[/red]")
                failed_count += 1
            progress.update(task, advance=1)

    console.print(
        f"\n[green]Successfully deleted {deleted_count:,} duplicate files[/green]"
    )
    if failed_count > 0:
        console.print(f"[red]Failed to delete {failed_count:,} files[/red]")

    return total_files, unique_count, deleted_count


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

    This script scans a maildir folder structure (with cur/, new/, tmp/ subdirectories),
    identifies duplicate emails using Message-ID headers, keeps the first occurrence
    (by filename sort), and deletes subsequent duplicates.
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
