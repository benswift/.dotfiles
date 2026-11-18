#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run bin/anu-publications-sync over a cached copy of tiny index pages.

The cache stands in for ANU's pages and the PDFs they link (the tool reads it
instead of fetching), and the PDFs are generated here, so these check what a
sync writes: a record per wanted document and none for the page's other PDFs,
the handbook's sections named from the page's own menu, layout kept for a fee
schedule, a rerun writing nothing, a dropped document's record removed, and a
refusal to sync when a page yields nothing.
"""

from __future__ import annotations

import hashlib
import os
import shutil
import subprocess
from pathlib import Path
from urllib.parse import quote

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "anu-publications-sync"

pytestmark = pytest.mark.skipif(not shutil.which("pdftotext"), reason="needs pdftotext")

GOV = "https://www.anu.edu.au/about/governance"
HB = f"{GOV}/frameworks-disclosures/anu-policy-handbooks"
FEES = "https://www.anu.edu.au/students/program-administration/fees-payments/fees-schedules"
CDN = "https://cdn.example/files"
SAFELINK = (
    "https://aus01.safelinks.protection.outlook.com/?url="
    "https%3A%2F%2Fcdn.example%2Ffiles%2Fold%2FAB%2520Charter.pdf&data=05"
)


def pdf(*lines: str) -> bytes:
    """A one-page PDF whose text is `lines`, one per row."""
    rows = "".join(f"({line}) Tj 0 -16 Td " for line in lines)
    stream = f"BT /F1 10 Tf 40 700 Td {rows}ET"
    objects = [
        "<< /Type /Catalog /Pages 2 0 R >>",
        "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
        (
            "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
            "/Contents 4 0 R /Resources << /Font << /F1 5 0 R >> >> >>"
        ),
        f"<< /Length {len(stream)} >>\nstream\n{stream}\nendstream",
        "<< /Type /Font /Subtype /Type1 /BaseFont /Courier >>",
    ]
    out = "%PDF-1.4\n"
    offsets = []
    for i, body in enumerate(objects, 1):
        offsets.append(len(out))
        out += f"{i} 0 obj\n{body}\nendobj\n"
    xref = len(out)
    out += f"xref\n0 {len(objects) + 1}\n0000000000 65535 f \n"
    out += "".join(f"{o:010d} 00000 n \n" for o in offsets)
    out += f"trailer\n<< /Size {len(objects) + 1} /Root 1 0 R >>\nstartxref\n{xref}\n%%EOF\n"
    return out.encode("latin-1")


def accred_url(n: int, title: str) -> str:
    return f"{CDN}/2026-01/" + quote(f"Accred {n}. {title}.pdf")


def key(url: str, suffix: str) -> str:
    return hashlib.sha1(url.encode()).hexdigest()[:16] + suffix


def put(
    cache: Path, url: str, data: bytes | str, suffix: str, modified: str = ""
) -> None:
    raw = data.encode() if isinstance(data, str) else data
    (cache / key(url, suffix)).write_bytes(raw)
    (cache / (key(url, suffix) + ".modified")).write_text(modified + "\n")


def page(*anchors: tuple[str, str]) -> str:
    links = "".join(f'<a href="{h}">{t}</a>' for h, t in anchors)
    return f"<html><body>{links}</body></html>"


def accreditation_page(sections: dict[int, str]) -> str:
    path = "/about/governance/frameworks-disclosures/anu-policy-handbooks"
    menu = [
        (f"{path}/academic-accreditation-handbook/{n}", f"{n}. {t}")
        for n, t in sections.items()
    ]
    pdfs = [(accred_url(n, t), "PDF") for n, t in sections.items()]
    return page(*menu, *pdfs)


SECTIONS = {
    2: "Accreditation of AQF Curriculum",
    3: "Program design",
    4: "Major, minor and specialisation design",
    5: "Course design",
    6: "Non-AQF accreditation: Professional development offerings",
    7: "Non-AQF accreditation: Microcredentials",
    8: "Non-AQF accreditation: Personal Development offerings",
}
BOARD = f"{GOV}/committees/academic-board"
COUNCIL = f"{GOV}/committees/council"
TRAVEL = f"{HB}/travel-policy-handbook"
ACCRED = f"{HB}/academic-accreditation-handbook"


def populate(cache: Path, min_sections: dict[int, str] = SECTIONS) -> None:
    put(cache, ACCRED, accreditation_page(min_sections), ".html")
    for n, t in min_sections.items():
        put(
            cache,
            accred_url(n, t),
            pdf(f"{n}. {t}", "1. Clause one."),
            ".pdf",
            "2026-01-05",
        )
    put(
        cache,
        TRAVEL,
        page(
            (f"{CDN}/t.pdf", "ANU Travel Handbook (PDF, 2 MB)"),
            ("https://other/t.pdf", "Travel Handbook"),
        ),
        ".html",
    )
    put(cache, f"{CDN}/t.pdf", pdf("Travel rules."), ".pdf", "2026-04-28")
    put(
        cache,
        FEES,
        page(
            (f"{CDN}/f26.pdf", "Schedule of Tuition Fees 2026 (PDF, 1 KB)"),
            (f"{CDN}/f27.pdf", "Schedule of Tuition Fees 2027 (PDF, 1 KB)"),
        ),
        ".html",
    )
    put(cache, f"{CDN}/f26.pdf", pdf("Band     Fee", "1        $46,680"), ".pdf")
    put(
        cache,
        f"{CDN}/f27.pdf",
        pdf("Band     Fee", "1        $47,000"),
        ".pdf",
        "2026-09-29",
    )
    put(
        cache,
        COUNCIL,
        page(
            (f"{CDN}/m.pdf", "Council Minutes 1/2026 (PDF, 1 MB)"),
            (f"{CDN}/c.pdf", "Council Charter - Approved 30 May 2025 (PDF, 6 KB)"),
            (
                f"{CDN}/so.pdf",
                "Standing Orders of the ANU Council and Council Committees (PDF, 6 KB)",
            ),
        ),
        ".html",
    )
    put(cache, f"{CDN}/c.pdf", pdf("Charter text."), ".pdf")
    put(cache, f"{CDN}/so.pdf", pdf("Standing orders text."), ".pdf")
    # Running text links a stale charter through a safelink; the download wins.
    put(
        cache,
        BOARD,
        page(
            (SAFELINK, "Academic Board Charter 2026"),
            (f"{CDN}/ab.pdf", "2026 Academic Board Charter_APPROVED.pdf (PDF, 2 KB)"),
        ),
        ".html",
    )
    put(cache, f"{CDN}/ab.pdf", pdf("Board charter text."), ".pdf")
    put(
        cache,
        "https://www.anu.edu.au/anu-policy-handbooks/",
        page(
            (f"{ACCRED}", "Academic Accreditation Handbook"),
            (f"{HB}/new-handbook", "New Handbook"),
        ),
        ".html",
    )


@pytest.fixture
def setup(tmp_path: Path) -> dict[str, Path]:
    cache = tmp_path / "cache"
    cache.mkdir()
    populate(cache)
    vault = tmp_path / "vault"
    vault.mkdir()
    return {"cache": cache, "vault": vault}


def sync(setup: dict[str, Path]) -> subprocess.CompletedProcess[str]:
    env = {**os.environ, "NO_COLOR": "1", "FORCE_COLOR": ""}
    return subprocess.run(
        [
            str(SCRIPT),
            "--vault",
            str(setup["vault"]),
            "--cache",
            str(setup["cache"]),
            "--delay",
            "0",
        ],
        check=False,
        env=env,
        capture_output=True,
        text=True,
    )


def names(setup: dict[str, Path]) -> list[str]:
    return sorted(p.name for p in (setup["vault"] / "publications").iterdir())


def test_writes_a_record_per_wanted_document(setup: dict[str, Path]) -> None:
    result = sync(setup)
    assert result.returncode == 0, result.stderr
    assert "13 documents: 13 files written" in result.stdout
    assert len(names(setup)) == 13
    assert "academic-board-charter.md" in names(setup)
    assert (
        "academic-accreditation-handbook-6-non-aqf-accreditation-"
        "professional-development-offerings.md" in names(setup)
    )
    assert "standing-orders-of-the-anu-council-and-council-committees.md" in names(
        setup
    )


def test_record_fields(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    pubs = setup["vault"] / "publications"
    section = (pubs / "academic-accreditation-handbook-3-program-design.md").read_text()
    assert section.startswith(
        "---\ntitle: Academic Accreditation Handbook 3. Program design\n"
        "kind: handbook\n"
        f"source: {accred_url(3, 'Program design')}\n"
        f"anu_page: {ACCRED}\n"
        "is_part_of: Academic Accreditation Handbook\n"
        "date_modified: '2026-01-05'\n"
        "encoding_format: pdf\n---\n"
    )
    assert "1. Clause one." in section
    travel = (pubs / "anu-travel-handbook.md").read_text()
    assert "is_part_of" not in travel and "kind: handbook" in travel
    # no Last-Modified from the server means no date_modified
    assert "date_modified" not in (pubs / "council-charter.md").read_text()
    assert (
        "kind: standing-orders"
        in (
            pubs / "standing-orders-of-the-anu-council-and-council-committees.md"
        ).read_text()
    )
    # the download's PDF, not the stale safelinked one
    board = (pubs / "academic-board-charter.md").read_text()
    assert f"source: {CDN}/ab.pdf" in board and "Board charter text." in board


def test_fee_schedule_keeps_its_layout(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    fees = (
        setup["vault"] / "publications" / "schedule-of-tuition-fees-2027.md"
    ).read_text()
    body = fees.split("---\n", 2)[2]
    assert body.startswith("\n```text\n") and body.endswith("```\n")
    assert "$47,000" in body


def test_rerun_writes_nothing_and_drops_a_dropped_document(
    setup: dict[str, Path],
) -> None:
    assert sync(setup).returncode == 0
    again = sync(setup)
    assert "0 files written, 0 removed" in again.stdout
    assert (
        "handbook not mirrored: https://www.anu.edu.au/anu-policy-handbooks/new-handbook"
        not in again.stdout
    )
    put(
        setup["cache"],
        FEES,
        page((f"{CDN}/f27.pdf", "Schedule of Tuition Fees 2027 (PDF, 1 KB)")),
        ".html",
    )
    result = sync(setup)
    assert result.returncode == 0, result.stderr
    assert "0 files written, 1 removed" in result.stdout
    assert "schedule-of-tuition-fees-2026.md" not in names(setup)


def test_reports_other_handbooks(setup: dict[str, Path]) -> None:
    result = sync(setup)
    assert f"handbook not mirrored: {HB}/new-handbook" in result.stdout


def test_refuses_when_a_page_yields_nothing(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    put(setup["cache"], FEES, page(("/other", "Fees")), ".html")
    result = sync(setup)
    assert result.returncode != 0
    assert "yields 0 documents, expected at least 1" in result.stderr
    assert len(names(setup)) == 13  # nothing was deleted


def test_refuses_a_handbook_missing_its_sections(setup: dict[str, Path]) -> None:
    put(setup["cache"], ACCRED, page(("/x", "nothing")), ".html")
    result = sync(setup)
    assert result.returncode != 0
    assert "expected at least 7" in result.stderr
    assert not (setup["vault"] / "publications").exists()


def test_refuses_a_pdf_with_no_text(setup: dict[str, Path]) -> None:
    put(setup["cache"], f"{CDN}/t.pdf", pdf(), ".pdf")
    result = sync(setup)
    assert result.returncode != 0
    assert "ANU Travel Handbook: the PDF has no text" in result.stderr
    assert not (setup["vault"] / "publications").exists()


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-n", "auto", "-q"]))
