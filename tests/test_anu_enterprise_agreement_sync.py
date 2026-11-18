#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run bin/anu-enterprise-agreement-sync over a cached copy of a tiny site.

The cache stands in for ANU Services' index and its clause and schedule pages
(the tool reads it instead of fetching), so these check what a sync writes:
one record per clause or schedule with the agreement's title taken from
clause 1, lettered lists and salary tables converted, a rerun that writes
nothing, a dropped clause removed, and a refusal to sync on a short index.
"""

from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "anu-enterprise-agreement-sync"

pytestmark = pytest.mark.skipif(not shutil.which("pandoc"), reason="needs pandoc")

BASE = "/human-resources/enterprise-agreement"

PAGES = {
    "1-title-and-operation-of-the-agreement": (
        "1. Title and operation of the agreement",
        (
            "<p><strong>1.1. </strong>This Agreement will be known as The "
            "Australian National University Enterprise Agreement 2023 \u2013 2026, "
            "and will commence operation seven days after approval.</p>"
        ),
    ),
    "14-casual-employment": (
        "14. Casual employment",
        (
            "<p><strong>14.4. </strong>Casual employment is limited to:</p>"
            '<ol type="a"><li>\n\tshort term engagements;</li>'
            "<li>work for a student.</li></ol>"
        ),
    ),
    "schedule-1-academic-staff-salary-schedule": (
        "Schedule 1. Academic staff salary schedule",
        (
            "<p><strong>Annual salary ($)</strong></p><table><tbody><tr>"
            "<th>Level</th><th><p>2 February 2023</p><p>3.5% increase</p></th></tr>"
            '<tr><td colspan="2">Level A</td></tr>'
            "<tr><td><strong>A1</strong></td><td>\n78,940</td></tr></tbody></table>"
        ),
    ),
}


def page(heading: str, body: str) -> str:
    return (
        f"<html><body><h1>{heading}</h1><article>"
        f'<div class="field field--name-body">{body}</div></article></body></html>'
    )


def index(slugs: list[str]) -> str:
    links = "".join(f'<a href="{BASE}/{s}">{PAGES[s][0]}</a>' for s in slugs)
    other = f'<a href="{BASE}/enterprise-bargaining">Enterprise Bargaining</a>'
    return f"<html><body>{links}{other}</body></html>"


@pytest.fixture
def setup(tmp_path: Path) -> dict[str, Path]:
    cache = tmp_path / "cache"
    (cache / "pages").mkdir(parents=True)
    (cache / "index.html").write_text(index(list(PAGES)))
    for slug, (heading, body) in PAGES.items():
        (cache / "pages" / f"{slug}.html").write_text(page(heading, body))
    vault = tmp_path / "vault"
    vault.mkdir()
    return {"cache": cache, "vault": vault}


def sync(setup: dict[str, Path], *extra: str) -> subprocess.CompletedProcess[str]:
    env = {
        **os.environ,
        # CI forces colour, which would split the messages the tests look for.
        "NO_COLOR": "1",
        "FORCE_COLOR": "",
    }
    return subprocess.run(
        [str(SCRIPT), "--vault", str(setup["vault"]), "--cache", str(setup["cache"])]
        + ["--delay", "0", "--min-clauses", "1", *extra],
        check=False,
        env=env,
        capture_output=True,
        text=True,
    )


def test_writes_a_record_per_page(setup: dict[str, Path]) -> None:
    result = sync(setup)
    assert result.returncode == 0, result.stderr
    folder = setup["vault"] / "enterprise-agreement"
    assert sorted(p.name for p in folder.iterdir()) == [
        "1-title-and-operation-of-the-agreement.md",
        "14-casual-employment.md",
        "schedule-1-academic-staff-salary-schedule.md",
    ]
    clause = (folder / "14-casual-employment.md").read_text()
    assert clause.startswith(
        "---\ntitle: Casual employment\nidentifier: '14'\nkind: clause\n"
        "is_part_of: The Australian National University Enterprise Agreement 2023-2026\n"
        f"source: https://services.anu.edu.au{BASE}/14-casual-employment\n---\n"
    )
    schedule = (folder / "schedule-1-academic-staff-salary-schedule.md").read_text()
    assert "title: Academic staff salary schedule\nidentifier: Schedule 1\n" in schedule
    assert "kind: schedule\n" in schedule


def test_lettered_lists_and_tables(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    folder = setup["vault"] / "enterprise-agreement"
    clause = (folder / "14-casual-employment.md").read_text().split("---\n", 2)[2]
    assert clause.strip().splitlines() == [
        "**14.4.** Casual employment is limited to:",
        "",
        "- (a) short term engagements;",
        "- (b) work for a student.",
    ]
    schedule = (folder / "schedule-1-academic-staff-salary-schedule.md").read_text()
    assert (
        "| Level | 2 February 2023<br>3.5% increase |\n"
        "|---------|--------------------------------|\n"
        "| Level A | |\n"
        "| **A1** | 78,940 |\n"
    ) in schedule


def test_rerun_writes_nothing_and_drops_a_dropped_clause(
    setup: dict[str, Path],
) -> None:
    assert sync(setup).returncode == 0
    again = sync(setup)
    assert "0 files written, 0 removed" in again.stdout
    (setup["cache"] / "index.html").write_text(
        index(["1-title-and-operation-of-the-agreement", "14-casual-employment"])
    )
    result = sync(setup)
    assert "0 files written, 1 removed" in result.stdout
    folder = setup["vault"] / "enterprise-agreement"
    assert not (folder / "schedule-1-academic-staff-salary-schedule.md").exists()


def test_refuses_a_short_index(setup: dict[str, Path]) -> None:
    result = sync(setup, "--min-clauses", "5")
    assert result.returncode != 0
    assert "lists only 3 pages" in result.stdout + result.stderr
    assert not (setup["vault"] / "enterprise-agreement").exists()


def test_refuses_a_page_whose_heading_differs(setup: dict[str, Path]) -> None:
    (setup["cache"] / "pages" / "14-casual-employment.html").write_text(
        page("14. Something else", "<p>x</p>")
    )
    result = sync(setup)
    assert result.returncode != 0
    assert "heading is" in result.stdout + result.stderr
    assert not (setup["vault"] / "enterprise-agreement").exists()


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-n", "auto", "-q"]))
