"""CLI for querying the student database."""

import json
import sys
from pathlib import Path
from typing import Annotated, Optional

import typer
from rich.console import Console

from mail_utils.studentdb import StudentDB

console = Console()
app = typer.Typer(
    help="Query the PhD student database.",
    add_completion=False,
    no_args_is_help=True,
)


def load_db(file: Path | None) -> StudentDB:
    try:
        if file:
            return StudentDB.from_file(file)
        return StudentDB.from_nb()
    except ValueError as e:
        console.print(f"[red]Error loading database: {e}[/red]")
        raise typer.Exit(1)
    except Exception as e:
        console.print(f"[red]Failed to load database: {e}[/red]")
        raise typer.Exit(1)


@app.command()
def students(
    status: Annotated[
        Optional[str],
        typer.Option("--status", "-s", help="Filter by status (e.g., confirmed, pre-confirmation)"),
    ] = None,
    file: Annotated[
        Optional[Path],
        typer.Option("--file", "-f", help="Path to database file (default: load from nb)"),
    ] = None,
) -> None:
    """List students with denormalised supervisor/panel information."""
    db = load_db(file)
    results = db.students(status=status)

    output = [s.model_dump(exclude_none=True) for s in results]
    json.dump(output, sys.stdout, indent=2)
    sys.stdout.write("\n")


@app.command()
def people(
    file: Annotated[
        Optional[Path],
        typer.Option("--file", "-f", help="Path to database file (default: load from nb)"),
    ] = None,
) -> None:
    """List all people in the database."""
    db = load_db(file)
    results = db.people()

    output = {k: v.model_dump(exclude_none=True) for k, v in results.items()}
    json.dump(output, sys.stdout, indent=2)
    sys.stdout.write("\n")


if __name__ == "__main__":
    app()
