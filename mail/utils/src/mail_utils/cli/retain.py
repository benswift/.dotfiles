"""Copy ageing Exchange mail into the local cold store (see mail_utils.retain)."""

from collections import Counter
from datetime import UTC, datetime, timedelta
from pathlib import Path

import typer

from mail_utils.retain import COLD_ROOT, MAILDIR, MIN_AGE, capture, plan

app = typer.Typer()


@app.command()
def retain(
    source: Path = typer.Option(
        MAILDIR,
        help="Root holding <account>/<folder> maildirs to sweep",
    ),
    cold: Path = typer.Option(
        COLD_ROOT,
        help="Cold store root; messages land in <cold>/<account>/<folder>",
    ),
    min_age_days: int = typer.Option(
        MIN_AGE.days, help="Capture messages whose Date is at least this old"
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", "-n", help="Report what would be copied, copy nothing"
    ),
) -> None:
    """Copy Exchange mail approaching ANU's two-year retention into the cold store.

    Idempotent: a message already in the cold store (by Message-ID, per
    account) is never copied again. --source also takes a restored backup laid
    out the same way, to recover mail the server has already expunged.

    Examples:

        mail-retain --dry-run

        mail-retain --source /tmp/restore/data/Maildir-imap
    """
    captures = plan(source, cold, datetime.now(UTC), timedelta(days=min_age_days))
    copied: Counter[Path] = Counter()
    vanished = 0
    for item in captures:
        if dry_run or capture(item):
            copied[item.dest.parent.parent] += 1
        else:
            vanished += 1

    verb = "would copy" if dry_run else "copied"
    for maildir, count in sorted(copied.items()):
        print(f"mail-retain: {verb} {count} into {maildir}")
    if vanished:
        print(f"mail-retain: {vanished} vanished mid-run; the next sweep retries")
    if not copied:
        print("mail-retain: nothing due")
