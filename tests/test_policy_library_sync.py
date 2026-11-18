#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run bin/policy-library-sync over a cached copy of a tiny library.

The cache stands in for the site (the tool reads it instead of fetching), and
a stub `wiz` stands in for validation, so these check what a sync writes:
records and originals, wiki links, relations stored once, and that a re-run
changes only what the library changed.
"""

from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "policy-library-sync"

pytestmark = pytest.mark.skipif(
    not (shutil.which("pandoc") and shutil.which("pdftotext")),
    reason="needs pandoc and pdftotext",
)


def page(
    ident: str,
    kind: str,
    title: str,
    body: str = "",
    related: tuple[str, ...] = (),
    download: bool = False,
) -> str:
    def row(label: str, value: str) -> str:
        return f"<tr><td>{label}</td><td>{value}</td></tr>"

    rel = "".join(f'<a href="/ppl/document/{r}">{r}</a>' for r in related)
    dl = f'<a href="/ppl/download/{ident}">Click to download</a>' if download else ""
    return f"""<html><head><meta name="keywords" content="leave, staff"></head><body>
{dl}<div class="doublewide" id="convertedcontent">{body}</div>
<div id="info_block"><table>
<tr><td><h3>Information</h3></td><td></td></tr>
{row("Title", title)}{row("Document Type", kind)}{row("Document Number", ident)}
{row("Version", "")}{row("Purpose", f"To set out {title}")}
{row("Audience", "Staff, Students, Staff")}{row("Category", "Administrative")}
{row("Topic/ SubTopic", "Staff - Leave &amp; Absences")}
{row("Effective Date", "1 Oct 2026")}{row("Next Review Date", "1 Oct 2031")}
{row("Responsible Officer:", '<a href="mailto:a@b">Chief People Officer</a>')}
{row("Approved By:", '<a href="mailto:a@b">Vice-Chancellor</a>')}
{row("Contact Area", "People and Culture Division")}
{row("Authority:", '<a href="https://legislation.gov.au/x">Fair Work Act 2009</a>')}
{row("Delegations", '<a href="https://example.org/d.pdf">238, 239</a>')}
</table></div>
<table><tr><td colspan="2"><h3>Related Content</h3></td></tr>
<tr><td>Related</td><td>{rel}</td></tr></table>
</body></html>"""


def index(docs: dict[str, tuple[str, str]]) -> str:
    rows = "".join(
        f'<tr><td><a href="/ppl/document/{i}">{t}</a></td><td>{k}</td></tr>'
        for i, (k, t) in docs.items()
    )
    return f"<html><body><table>{rows}</table></body></html>"


@pytest.fixture
def library(tmp_path: Path) -> dict[str, Path]:
    cache = tmp_path / "cache"
    (cache / "html").mkdir(parents=True)
    (cache / "originals").mkdir()
    form = cache / "originals" / "ANUP_000003"
    subprocess.run(
        ["pandoc", "-o", f"{form}.docx"],
        input=b"# Leave form\n\nSee https://policies.anu.edu.au/ppl/document/ANUP_000001\n",
        check=True,
    )
    Path(f"{form}.docx").rename(form)
    (cache / "originals" / "ANUP_000003.headers").write_text(
        "content-type: application/octet-stream\n"
        "content-disposition: attachment; filename*=UTF-8''Leave%20form.docx\n"
    )
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    (bin_dir / "wiz").write_text("#!/bin/sh\nexit 0\n")
    (bin_dir / "wiz").chmod(0o755)
    vault = tmp_path / "vault"
    vault.mkdir()
    return {"cache": cache, "vault": vault, "bin": bin_dir}


def write_library(cache: Path, policy_title: str, with_form: bool) -> None:
    docs = {
        "ANUP_000001": ("Policy", policy_title),
        "ANUP_000002": ("Procedure", "Staff leave"),
    }
    if with_form:
        docs["ANUP_000003"] = ("Form", "Leave application")
    (cache / "index.html").write_text(index(docs))
    for f in (cache / "html").glob("*.html"):
        f.unlink()
    pages = {
        "ANUP_000001": page(
            "ANUP_000001",
            "Policy",
            policy_title,
            "<h1>Policy: x</h1><h2>Purpose</h2><ol><li>Staff may take leave, per the "
            '<a href="https://policies.anu.edu.au/ppl/document/ANUP_000002">leave procedure</a>.'
            "</li><li>Leave must be:</li></ol>"
            '<ol type="a"><li>approved</li></ol><ol type="i"><li>in writing</li></ol>'
            '<ol start="2" type="a"><li>recorded</li></ol><ol start="3"><li>See the '
            '<a href="/ppl/document/ANUP_000099">old policy</a>.</li></ol>',
            related=("ANUP_000002",),
        ),
        "ANUP_000002": page(
            "ANUP_000002",
            "Procedure",
            "Staff leave",
            '<h1>Procedure: x</h1><p>See the <a href="/ppl/document/ANUP_000001">policy</a>'
            ' and <a href="https://aus01.safelinks.protection.outlook.com/?url='
            'https%3A%2F%2Fexample.org%2Fleave&amp;data=x">this page</a>.</p>'
            "<table><tr><td>Kind</td>"
            '<td><a href="/ppl/document/ANUP_000001">Policy</a></td></tr></table>',
            related=("ANUP_000001",),
        ),
    }
    if with_form:
        pages["ANUP_000003"] = page(
            "ANUP_000003",
            "Form",
            "Leave application",
            related=("ANUP_000002",),
            download=True,
        )
    for ident, html in pages.items():
        (cache / "html" / f"{ident}.html").write_text(html)
    (cache / "html" / "ANUP_000099.html.404").touch()  # a retired document


def sync(lib: dict[str, Path]) -> str:
    env = {**os.environ, "PATH": f"{lib['bin']}{os.pathsep}{os.environ['PATH']}"}
    result = subprocess.run(
        [
            SCRIPT,
            "--vault",
            lib["vault"],
            "--cache",
            lib["cache"],
            "--min-documents",
            "1",
        ],
        capture_output=True,
        text=True,
        env=env,
        check=True,
    )
    return result.stdout


def test_first_sync_writes_records_links_and_originals(
    library: dict[str, Path],
) -> None:
    write_library(library["cache"], "Staff leave (Under review)", with_form=True)
    sync(library)
    vault = library["vault"]

    policy = (vault / "policies" / "staff-leave.md").read_text()
    assert "status: under-review" in policy
    assert "title: Staff leave\n" in policy
    assert "valid_from: '2026-10-01'" in policy
    assert "- Fair Work Act 2009" in policy
    assert "audience:\n- Staff\n- Students\n" in policy
    assert "Policy: x" not in policy
    assert "[[procedures/staff-leave|leave procedure]]" in policy
    assert (
        "2.  Leave must be:\n    - a. approved\n      - i. in writing\n    - b. recorded"
        in policy
    )
    assert (
        "[old policy](https://policies.anu.edu.au/ppl/document/ANUP_000099)" in policy
    )
    assert "is_related_to" not in policy  # stored on the procedure instead

    procedure = (vault / "procedures" / "staff-leave.md").read_text()
    assert "is_related_to:\n- '[[policies/staff-leave]]'" in procedure
    assert "[[policies/staff-leave|policy]]" in procedure
    assert "(https://example.org/leave)" in procedure
    assert "| [[policies/staff-leave]] |" in procedure  # no alias inside a table

    form = (vault / "forms" / "leave-application.md").read_text()
    assert "associated_media: '[[files/forms/leave-application.docx]]'" in form
    assert "[[policies/staff-leave]]" in form
    assert (vault / "files" / "forms" / "leave-application.docx").is_file()


def test_unchanged_library_writes_nothing(library: dict[str, Path]) -> None:
    write_library(library["cache"], "Staff leave", with_form=True)
    sync(library)
    assert "0 files written, 0 removed" in sync(library)


def test_retitled_and_dropped_documents_follow_the_library(
    library: dict[str, Path],
) -> None:
    write_library(library["cache"], "Staff leave", with_form=True)
    sync(library)
    write_library(library["cache"], "Leave entitlements", with_form=False)
    sync(library)
    vault = library["vault"]

    assert not (vault / "policies" / "staff-leave.md").exists()
    assert (vault / "policies" / "leave-entitlements.md").is_file()
    procedure = (vault / "procedures" / "staff-leave.md").read_text()
    assert "[[policies/leave-entitlements|policy]]" in procedure
    assert not (vault / "forms").exists()
    assert not (vault / "files").exists()


def test_a_short_index_aborts_without_writing(library: dict[str, Path]) -> None:
    write_library(library["cache"], "Staff leave", with_form=True)
    env = {**os.environ, "PATH": f"{library['bin']}{os.pathsep}{os.environ['PATH']}"}
    result = subprocess.run(
        [SCRIPT, "--vault", library["vault"], "--cache", library["cache"]],
        check=False,
        capture_output=True,
        text=True,
        env=env,
    )
    assert result.returncode != 0
    assert "lists only 3 documents" in result.stderr
    assert not any(library["vault"].iterdir())
