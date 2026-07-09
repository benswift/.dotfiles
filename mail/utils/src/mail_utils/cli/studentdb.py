"""CLI for querying the student database."""

import json
import sys
from pathlib import Path
from typing import Annotated

import typer
from rich.console import Console

from mail_utils.studentdb import StudentDB

console = Console()
app = typer.Typer(
    help="""Query the PhD student database.

    Outputs JSON to stdout, designed to pipe into jq or mail-compose.

    `students` DEFAULTS TO THE CONVENOR COHORT: SOCY students still in
    candidature. That covers the common case (emailing the school's current
    HDR cohort) but silently omits completed students and Ben's students in
    other schools. Pass --all to see every student in the database.

    Examples:

        # The current SOCY cohort (the default)
        student-db students

        # Everything: all schools, all statuses
        student-db students --all

        # Students Ben supervises, chairs, or sits on the panel for
        student-db students --all --ben-role any

        # ...only the ones where he's primary supervisor
        student-db students --all --ben-role primary

        # Pipe to mail-compose for batch emails
        student-db students --status confirmed | \\
            mail-compose -f phdconvenor --data - --to '{{email}}' \\
            --subject 'Hello {{preferred_name}}' --template body.md --send

        # List all people (supervisors, panel members, contacts)
        student-db people
    """,
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
        str | None,
        typer.Option(
            "--status",
            "-s",
            help="Filter by status (e.g., confirmed, completed). Implies --include-inactive.",
        ),
    ] = None,
    school: Annotated[
        str,
        typer.Option("--school", help="Filter by school (e.g., SOCY, SOCO, RSCS)"),
    ] = "SOCY",
    ben_role: Annotated[
        str | None,
        typer.Option(
            "--ben-role",
            help="Only students Ben is involved with: any, primary, panel-chair, associate, crp-chair",
        ),
    ] = None,
    all_: Annotated[
        bool,
        typer.Option(
            "--all", "-a", help="No default filters: every school, every status"
        ),
    ] = False,
    all_schools: Annotated[
        bool, typer.Option("--all-schools", help="Ignore the SOCY school default")
    ] = False,
    include_inactive: Annotated[
        bool,
        typer.Option("--include-inactive", help="Include completed and withdrawn"),
    ] = False,
    file: Annotated[
        Path | None,
        typer.Option(
            "--file", "-f", help="Path to database file (default: load from nb)"
        ),
    ] = None,
) -> None:
    """List students with denormalised supervisor/panel information.

    Defaults to SOCY students still in candidature --- see `student-db --help`.
    """
    db = load_db(file)
    # An explicit --status is itself a status filter, so don't also apply the
    # active-only default on top of it and quietly return nothing for
    # `--status completed`.
    results = db.students(
        status=status,
        school=None if (all_ or all_schools) else school,
        ben_role=ben_role,
        active_only=not (all_ or include_inactive or status),
    )

    output = [s.model_dump(exclude_none=True) for s in results]
    json.dump(output, sys.stdout, indent=2)
    sys.stdout.write("\n")


@app.command()
def people(
    file: Annotated[
        Path | None,
        typer.Option(
            "--file", "-f", help="Path to database file (default: load from nb)"
        ),
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
