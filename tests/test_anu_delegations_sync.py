#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run bin/anu-delegations-sync over a cached copy of a tiny register.

The cache stands in for ANU's delegations framework page, the register PDF it
links and the Register's titles API (the tool reads it instead of fetching).
The PDF is written here by hand, two pages laid out like the real one with the
PDF standard fonts, so these check what a sync makes of the register's shape:
one record per delegation with its heading, clusters and their conditions, an
unnumbered delegate row under a cluster, a committee with no profile, a note
in the heading column, references split and their URLs rejoined, a Register
ID linked to its legislation record directly or, for a superseded instrument,
to its successor's by name, an unknown or unmatched ID kept as text, a
heading carried to the next page, a rerun writing nothing, stale records
removed, and refusals to sync a short or unreadable register.
"""

from __future__ import annotations

import json
import os
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "anu-delegations-sync"
PDF_URL = "https://example.cloudfront.net/files/2026-05/by-category.pdf"
PAGE = f"""<html><body><ul>
<li><a href="{PDF_URL}">Delegations by Category (PDF, 776.96 KB)</a></li>
<li><a href="https://example.cloudfront.net/files/other.pdf">Stocktake</a></li>
</ul></body></html>"""

FONTS = {"R": "Helvetica", "B": "Helvetica-Bold", "I": "Helvetica-BoldOblique"}
# (x, top, font, size, text), top measured down from the page's top edge.
Text = tuple[float, float, str, float, str]


def make_pdf(pages: list[list[Text]], height: float = 595.2) -> bytes:
    """A landscape PDF placing each text where it says."""
    objects: list[bytes] = [
        b"<< /Type /Catalog /Pages 2 0 R >>",
        b"",  # the page tree, once the pages are numbered
        *(
            f"<< /Type /Font /Subtype /Type1 /BaseFont /{name} "
            "/Encoding /WinAnsiEncoding >>".encode()
            for name in FONTS.values()
        ),
    ]
    fonts = " ".join(f"/{key} {i} 0 R" for i, key in enumerate(FONTS, 3))
    kids = []
    for texts in pages:
        ops = []
        for x, top, font, size, text in texts:
            escaped = text.replace("\\", "\\\\").replace("(", "\\(").replace(")", "\\)")
            y = height - top - size * 0.8
            ops.append(f"BT /{font} {size} Tf {x} {y:.2f} Td ({escaped}) Tj ET")
        stream = "\n".join(ops).encode("cp1252")
        objects.append(
            b"<< /Length %d >>\nstream\n%s\nendstream" % (len(stream), stream)
        )
        objects.append(
            f"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 841.68 {height}] "
            f"/Resources << /Font << {fonts} >> >> /Contents {len(objects)} 0 R >>".encode()
        )
        kids.append(f"{len(objects)} 0 R")
    objects[1] = (
        f"<< /Type /Pages /Kids [{' '.join(kids)}] /Count {len(kids)} >>".encode()
    )
    out = bytearray(b"%PDF-1.4\n")
    offsets = []
    for i, body in enumerate(objects, 1):
        offsets.append(len(out))
        out += b"%d 0 obj\n%s\nendobj\n" % (i, body)
    xref = len(out)
    out += b"xref\n0 %d\n0000000000 65535 f \n" % (len(objects) + 1)
    out += b"".join(b"%010d 00000 n \n" % o for o in offsets)
    out += b"trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n" % (
        len(objects) + 1,
        xref,
    )
    return bytes(out)


def header(edges: list[float]) -> list[Text]:
    """A page's title and header row, with columns starting at `edges`."""
    (subid, sub, trans, del_id, cluster, desc, kind, date, pid, pdesc, ptype) = edges[
        :11
    ]
    ptdesc, ref, url = edges[11:]
    h = 5.9
    return [
        (350, 14, "B", 7.8, "Australian National University"),
        (341, 34, "R", 7.8, "Delegations Category ID: EXEC01"),
        (344, 44, "R", 7.8, "Category Description: Executive"),
        (trans + 0.3, 89, "I", h, "Trans"),
        (subid, 96.5, "I", h, "Sub-"),
        (sub, 96.5, "I", h, "Sub-Category"),
        (trans, 96.5, "I", h, "ferrab"),
        (del_id, 96.5, "I", h, "Del"),
        (kind, 96.5, "I", h, "Delegation"),
        (date, 96.5, "I", h, "Effective"),
        (pdesc, 96.5, "I", h, "Profile ID"),
        (ptdesc, 96.5, "I", h, "Profile Type"),
        (ref, 96.5, "I", h, "Source/Reference"),
        (url, 96.5, "I", h, "Source/Reference"),
        (subid, 104, "I", h, "Category ID"),
        (sub, 104, "I", h, "Description"),
        (trans + 6, 104, "I", h, "le"),
        (del_id + 1.7, 104, "I", h, "ID"),
        (cluster, 104, "I", h, "Cluster"),
        (desc, 104, "I", h, "Delegation Description"),
        (kind, 104, "I", h, "Type"),
        (date, 104, "I", h, "Date"),
        (pid, 104, "I", h, "Profile ID"),
        (pdesc, 104, "I", h, "Description"),
        (ptype, 104, "I", h, "Profile Type"),
        (ptdesc, 104, "I", h, "Description"),
        (ref, 104, "I", h, "Description"),
        (url, 104, "I", h, "URL"),
    ]


EDGES: list[float] = [
    18,
    57,
    113,
    134,
    149,
    174,
    356,
    394,
    431,
    475,
    534,
    581,
    643,
    705,
]
# The second page's columns sit elsewhere, as the real register's do.
EDGES_2: list[float] = [
    18,
    62,
    118,
    139,
    154,
    179,
    361,
    399,
    436,
    480,
    539,
    586,
    648,
    710,
]


def row(
    edges: list[float], top: float, cells: dict[int, list[str]], font: str = "R"
) -> list[Text]:
    """A row's cells, by column index, each a list of wrapped lines."""
    return [
        (edges[col] + 0.1, top + 8 * i, font, 6.5, line)
        for col, lines in cells.items()
        for i, line in enumerate(lines)
    ]


def register() -> bytes:
    e = EDGES
    page_1 = [
        *header(e),
        (e[0], 112, "B", 6.5, "POWERS"),
        (e[1], 112, "I", 6.5, "Powers"),
        (e[3] + 3.5, 128, "B", 6.5, "2"),
        *row(
            e,
            128,
            {
                5: [
                    "Subject to the Council Charter, determine",
                    "and authorise the release of policy",
                ]
            },
            "I",
        ),
        *row(
            e,
            128,
            {2: ["Y"], 6: ["DS"], 7: ["9/04/2021"], 8: ["VC"], 9: ["Vice-Chancellor"]},
        ),
        *row(e, 128, {10: ["EXEC"], 11: ["Executive"], 12: ["Council Charter"]}),
        *row(
            e,
            128,
            {
                13: [
                    "https://www.anu.edu.",
                    "au/files/committee/C",
                    "ouncil%20Charter.pdf",
                ]
            },
        ),
        *row(
            e,
            160,
            {
                12: ["GovS 66"],
                13: ["https://www.legislatio", "n.gov.au/F2023L008", "67"],
            },
        ),
        (e[0], 190, "B", 6.5, "HONORARY"),
        *row(e, 190, {1: ["Honorary and", "Ceremonial"]}, "I"),
        (e[3] + 3.5, 214, "B", 6.5, "5"),
        *row(e, 214, {5: ["Act for the University in intellectual property"]}, "I"),
        *row(
            e,
            214,
            {
                2: ["Y"],
                6: ["DL"],
                7: ["8/02/2023"],
                12: ["Power re-delegated", "by VC to DVCRI"],
            },
        ),
        *row(e, 238, {4: ["1"], 5: ["For matters relating to research"], 8: ["DVCRI"]}),
        *row(
            e,
            238,
            {9: ["DVC (Research &", "Innovation)"], 10: ["EXEC"], 11: ["Executive"]},
        ),
        *row(
            e,
            238,
            {
                12: ["GovS 58, 66"],
                13: ["https://www.legislatio", "n.gov.au/F2023L008", "67"],
            },
        ),
        *row(
            e,
            262,
            {
                4: ["2"],
                5: ["Within available funds: Up to $1.5M"],
                6: ["DB"],
                8: ["D2"],
            },
        ),
        *row(
            e,
            262,
            {9: ["Delegation Band 2"], 10: ["BANDED"], 11: ["Banded Delegations"]},
        ),
        *row(
            e,
            270,
            {6: ["DS"], 8: ["CFO"], 9: ["Chief Finance", "Officer"], 10: ["ADMINDIR"]},
        ),
        *row(e, 270, {11: ["Admin Director"]}),
        # A note on its row, in the sub-category column.
        *row(e, 290, {0: ["Delegation", "removed", "1/01/2021"]}),
        (e[3] + 1.8, 290, "B", 6.5, "34"),
        *row(e, 290, {5: ["Set HECS fees"]}, "I"),
        *row(e, 290, {2: ["N"], 6: ["DB"], 7: ["1/01/1946"]}),
        *row(e, 290, {8: ["Council*"]}, "I"),
        (e[3], 318, "B", 6.5, "463"),
        *row(e, 318, {5: ["Determine academic University policy:"]}, "I"),
        *row(e, 318, {8: ["Academic", "Board*"]}, "I"),
        *row(e, 318, {2: ["N"], 6: ["DS"], 7: ["9/04/2021"], 12: ["ANU Act 18"]}),
        *row(
            e, 318, {13: ["https://www.legislatio", "n.gov.au/Series/C200", "4A04206"]}
        ),
        (
            e[0],
            350,
            "R",
            6.5,
            "* Position/Committee does not have a delegations profile",
        ),
    ]
    e = EDGES_2
    page_2 = [
        *header(e),
        (e[3] + 3.5, 112, "B", 6.5, "7"),
        *row(e, 112, {5: ["Approve ceremonial officers"]}, "I"),
        *row(
            e,
            112,
            {
                2: ["Y"],
                6: ["DB"],
                7: ["1/01/1946"],
                8: ["D1"],
                9: ["Delegation Band 1"],
            },
        ),
        *row(e, 112, {10: ["BANDED"], 11: ["Banded Delegations"]}),
        # An ID the Register doesn't know, then a title no record matches.
        *row(
            e,
            112,
            {12: ["FOI Act"], 13: ["https://www.legislation.gov.au/C2020C00110"]},
        ),
        *row(e, 120, {12: ["CWHandbookR 9"], 13: ["https://www.legislatio"]}),
        *row(e, 128, {13: ["n.gov.au/Details/F2013L02083"]}),
    ]
    return make_pdf([page_1, page_2])


# The register cites the Governance Statute 2023 (F2023L00867); the vault
# holds its successor, found through the Register's name for the old ID.
LEGISLATION = {
    "australian-national-university-governance-statute-2024": (
        "Australian National University (Governance) Statute 2024",
        "F2024L00724",
    ),
    "australian-national-university-act-1991": (
        "Australian National University Act 1991",
        "C2004A04206",
    ),
}
# The Register's titles API, as the cache holds it; null for an unknown ID.
TITLES = {
    "F2023L00867": {"name": "Australian National University (Governance) Statute 2023"},
    "F2013L02083": {"name": "Coursework Handbook Rules 2013"},
    "C2020C00110": None,
}


@pytest.fixture
def setup(tmp_path: Path) -> dict[str, Path]:
    cache = tmp_path / "cache"
    (cache / "pdf").mkdir(parents=True)
    (cache / "page.html").write_text(PAGE)
    (cache / "pdf" / "files-2026-05-by-category.pdf").write_bytes(register())
    vault = tmp_path / "vault"
    (vault / "legislation").mkdir(parents=True)
    (cache / "titles").mkdir()
    for ident, title in TITLES.items():
        (cache / "titles" / f"{ident}.json").write_text(json.dumps(title))
    for name, (title, ident) in LEGISLATION.items():
        (vault / "legislation" / f"{name}.md").write_text(
            f"---\ntitle: {title}\nidentifier: {ident}\nregister_id: {ident}\n---\n\nText.\n"
        )
    return {"cache": cache, "vault": vault}


def sync(setup: dict[str, Path], *extra: str) -> subprocess.CompletedProcess[str]:
    # CI forces colour, which would split the messages the tests look for.
    env = {**os.environ, "NO_COLOR": "1", "FORCE_COLOR": ""}
    return subprocess.run(
        [str(SCRIPT), "--vault", str(setup["vault"]), "--cache", str(setup["cache"])]
        + ["--delay", "0", "--min-delegations", "1", *extra],
        check=False,
        env=env,
        capture_output=True,
        text=True,
    )


def record(setup: dict[str, Path], ident: str) -> tuple[str, str]:
    """A record's frontmatter and body."""
    _, fm, body = (
        (setup["vault"] / "delegations" / f"{ident}.md").read_text().split("---\n", 2)
    )
    return fm, body.strip()


def test_writes_a_record_per_delegation(setup: dict[str, Path]) -> None:
    result = sync(setup)
    assert result.returncode == 0, result.stderr
    assert "5 delegations: 5 files written, 0 removed" in result.stdout
    names = sorted(p.name for p in (setup["vault"] / "delegations").iterdir())
    assert names == ["2.md", "34.md", "463.md", "5.md", "7.md"]
    fm, body = record(setup, "2")
    assert fm == (
        "title: Subject to the Council Charter, determine and authorise the release of policy\n"
        "identifier: '2'\n"
        "category: Executive\n"
        "topic: Powers\n"
        "kind:\n- specialist\n"
        "transferable: true\n"
        "valid_from: '2021-04-09'\n"
        "is_based_on:\n"
        "- Council Charter\n"
        "- '[[legislation/australian-national-university-governance-statute-2024|GovS 66]]'\n"
        f"source: {PDF_URL}\n"
    )
    assert body.splitlines() == [
        "Subject to the Council Charter, determine and authorise the release of policy",
        "",
        "## Delegates",
        "",
        "- Vice-Chancellor (VC, Executive)",
        "",
        "## Sources",
        "",
        "- [Council Charter](https://www.anu.edu.au/files/committee/Council%20Charter.pdf)",
        "- [GovS 66](https://www.legislation.gov.au/F2023L00867)",
    ]


def test_clusters_and_their_delegates(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    fm, body = record(setup, "5")
    assert "topic: Honorary and Ceremonial\n" in fm
    assert "kind:\n- legislative\n- banded\n- specialist\n" in fm
    statute = "legislation/australian-national-university-governance-statute-2024"
    based_on = f"- Power re-delegated by VC to DVCRI\n- '[[{statute}|GovS 58, 66]]'\n"
    assert based_on in fm
    assert body.split("## Delegates\n\n")[1].splitlines() == [
        "- (1) For matters relating to research",
        (
            "  - DVC (Research & Innovation) (DVCRI, Executive, legislative); source: "
            "[GovS 58, 66](https://www.legislation.gov.au/F2023L00867)"
        ),
        "- (2) Within available funds: Up to $1.5M",
        "  - Delegation Band 2 (D2, Banded Delegations, banded)",
        "  - Chief Finance Officer (CFO, Admin Director, specialist)",
        "",
        "## Sources",
        "",
        "- Power re-delegated by VC to DVCRI",
    ]


def test_notes_committees_and_pages(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    fm, body = record(setup, "34")
    assert "valid_from: '1946-01-01'" in fm and "transferable: false" in fm
    assert body.splitlines() == [
        "Set HECS fees",
        "",
        "Note: Delegation removed 1/01/2021.",
        "",
        "## Delegates",
        "",
        "- Council (no delegations profile)",
    ]
    fm, body = record(setup, "463")
    assert fm.startswith("title: Determine academic University policy\n")
    assert (
        "- '[[legislation/australian-national-university-act-1991|ANU Act 18]]'" in fm
    )
    assert "- Academic Board (no delegations profile)" in body
    # Page 2 carries page 1's last heading; the footnote between isn't one.
    fm, body = record(setup, "7")
    assert "topic: Honorary and Ceremonial\n" in fm
    assert "- Delegation Band 1 (D1, Banded Delegations)" in body
    assert "is_based_on:\n- FOI Act\n- CWHandbookR 9\n" in fm
    assert body.endswith(
        "## Sources\n\n- [FOI Act](https://www.legislation.gov.au/C2020C00110)\n"
        "- [CWHandbookR 9](https://www.legislation.gov.au/Details/F2013L02083)"
    )


def test_rerun_writes_nothing_and_drops_stale_records(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    stale = setup["vault"] / "delegations" / "999.md"
    stale.write_text("old\n")
    result = sync(setup)
    assert result.returncode == 0
    assert "0 files written, 1 removed" in result.stdout
    assert not stale.exists()


def test_refuses_a_short_register(setup: dict[str, Path]) -> None:
    result = sync(setup, "--min-delegations", "10")
    assert result.returncode != 0
    assert "holds only 5 delegations" in result.stderr
    assert not (setup["vault"] / "delegations").exists()


@pytest.mark.parametrize(
    ("data", "message"),
    [
        (make_pdf([[(18, 100, "R", 6.5, "Nothing like a register")]]), "no header row"),
        (b"not a PDF at all", ""),
    ],
)
def test_refuses_an_unreadable_register(
    setup: dict[str, Path], data: bytes, message: str
) -> None:
    (setup["cache"] / "pdf" / "files-2026-05-by-category.pdf").write_bytes(data)
    result = sync(setup)
    assert result.returncode != 0
    assert message in result.stderr
    assert not (setup["vault"] / "delegations").exists()


def test_refuses_a_page_without_the_register(setup: dict[str, Path]) -> None:
    (setup["cache"] / "page.html").write_text("<html><body>Moved</body></html>")
    result = sync(setup)
    assert result.returncode != 0
    assert "links no Delegations by Category PDF" in result.stderr


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-n", "auto", "-q"]))
