"""Deduplicate email messages in a maildir folder based on Message-ID headers."""

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

from mail_utils.maildir import is_maildir, open_maildir

console = Console()
app = typer.Typer()


def find_duplicates(
    mbox, show_progress: bool = True
) -> tuple[dict[str, list[str]], int]:
    """Find duplicate messages in a maildir folder."""
    message_id_to_keys: dict[str, list[str]] = defaultdict(list)
    keys = list(mbox.keys())
    total_messages = len(keys)

    if total_messages == 0:
        return message_id_to_keys, 0

    def process_keys():
        for key in keys:
            try:
                msg = mbox[key]
                msg_id = msg.get("Message-ID")
                if msg_id:
                    message_id_to_keys[msg_id.strip()].append(key)
            except Exception as e:
                console.print(f"[yellow]Warning: Could not read message {key}: {e}[/yellow]")
            yield

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
            for _ in process_keys():
                progress.update(task, advance=1)
    else:
        for _ in process_keys():
            pass

    return message_id_to_keys, total_messages


def deduplicate(
    maildir_path: Path, dry_run: bool = False, verbose: bool = False
) -> tuple[int, int, int]:
    """Deduplicate messages in a maildir folder."""
    console.print(f"\n[bold]Scanning maildir: {maildir_path}[/bold]")

    try:
        mbox = open_maildir(maildir_path)
    except Exception as e:
        console.print(f"[red]Error opening maildir: {e}[/red]")
        return 0, 0, 0

    message_id_to_keys, total_messages = find_duplicates(mbox)

    if total_messages == 0:
        console.print("[yellow]No messages found in maildir[/yellow]")
        return 0, 0, 0

    unique_count = len(message_id_to_keys)
    duplicate_count = 0
    keys_to_delete = []

    for message_id, keys in message_id_to_keys.items():
        if len(keys) > 1:
            sorted_keys = sorted(keys)
            keys_to_delete.extend(sorted_keys[1:])
            duplicate_count += len(sorted_keys) - 1

            if verbose and len(sorted_keys) > 1:
                console.print(f"\n[cyan]Message-ID: {message_id}[/cyan]")
                console.print(f"  [green]Keeping:[/green] {sorted_keys[0]}")
                for key in sorted_keys[1:]:
                    console.print(f"  [red]Deleting:[/red] {key}")

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

    mbox.flush()
    mbox.close()

    console.print(f"\n[green]Successfully deleted {deleted_count:,} duplicate messages[/green]")
    if failed_count > 0:
        console.print(f"[red]Failed to delete {failed_count:,} messages[/red]")

    return total_messages, unique_count, deleted_count


@app.command()
def dedupe_command(
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
    """Deduplicate email messages in a maildir folder based on Message-ID headers."""
    try:
        if not is_maildir(maildir_path):
            console.print(
                f"[red]Error: {maildir_path} doesn't appear to be a maildir folder.[/red]\n"
                "Expected to find at least one of: cur/, new/, tmp/ subdirectories"
            )
            raise typer.Exit(code=1)

        total, unique, removed = deduplicate(maildir_path, dry_run, verbose)

        if total == 0:
            raise typer.Exit(code=1)

    except KeyboardInterrupt:
        console.print("\n[yellow]Interrupted by user[/yellow]")
        raise typer.Exit(code=130)


def main():
    app()


if __name__ == "__main__":
    main()
