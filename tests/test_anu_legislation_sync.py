#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run bin/anu-legislation-sync over a cached copy of a tiny index.

The cache stands in for ANU's legislation page, the Register's API and its
EPUBs (the tool reads it instead of fetching), and a stub `wiz` stands in for
validation, so these check what a sync writes: one record per title with the
Register's details, headings and provisions converted, a title found under
its "and" spelling, a cited title the index doesn't list, stale records removed, and a refusal to sync when a title
can't be found.
"""

from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess
import zipfile
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "anu-legislation-sync"

pytestmark = pytest.mark.skipif(not shutil.which("pandoc"), reason="needs pandoc")

RULE_HTML = """<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>x</title></head><body>
<p class="TOC3"><span>34</span><span>Place where program must be undertaken</span></p>
<h1><span>Part 3—Program requirements</span></h1>
<h3><span>34</span><a id="_Toc1"><span>Place where program must be undertaken</span></a></h3>
<h4 style="text-indent:-18pt"><span>(1)</span><span style="width:5pt; display:inline-block">&#xa0;</span><span>A student must undertake the program at a campus of the University.</span></h4>
<h5><span>(a)</span><span style="font:7pt 'Times New Roman'">&#xa0;&#xa0;</span><span>if the program is outside the University;</span></h5>
<h6><span>(i)</span><span>supervision is comparable.</span></h6>
</body></html>"""

ACT_HTML = """<html><body>
<p class="ActHead2"><span>Part 1—Preliminary</span></p>
<p class="ActHead5"><span class="CharSectno">5</span><span>&#xa0; </span><span>Functions of the University</span></p>
<p class="subsection"><span style="width:38pt; display:inline-block">&#xa0;</span><span>(1)</span><span style="width:5pt; display:inline-block">&#xa0;</span><span>The functions include the following:</span></p>
<p class="paragraph"><span>(a)</span><span style="width:5pt; display:inline-block">&#xa0;</span><span>advancing knowledge.</span></p>
<p class="TOC5"><span>5 Functions of the University</span></p>
</body></html>"""


def epub(html: str) -> bytes:
    path = Path(os.environ.get("TMPDIR", "/tmp")) / f"{os.getpid()}-{id(html)}.epub"
    with zipfile.ZipFile(path, "w") as z:
        z.writestr("mimetype", "application/epub+zip")
        z.writestr(
            "META-INF/container.xml",
            '<?xml version="1.0"?><container><rootfiles>'
            '<rootfile full-path="OEBPS/document.opf"/></rootfiles></container>',
        )
        z.writestr(
            "OEBPS/document.opf",
            '<?xml version="1.0"?><package><manifest>'
            '<item id="d1" href="document_1/document_1.html"/>'
            '<item id="css" href="document_1/styles.css"/>'
            '</manifest><spine><itemref idref="d1"/></spine></package>',
        )
        z.writestr("OEBPS/document_1/document_1.html", html)
    data = path.read_bytes()
    path.unlink()
    return data


def index(names: list[str]) -> str:
    links = "".join(
        f'<a href="/about/governance/legislation/l{i}">{n}</a>'
        for i, n in enumerate(names)
    )
    return f"<html><body>{links}</body></html>"


def key(name: str) -> str:
    return hashlib.sha1(name.encode()).hexdigest()[:12]


def register(
    cache: Path, name: str, ident: str, kind: str, html: str, compilation: int = 0
) -> None:
    (cache / "titles" / f"{key(name)}.json").write_text(
        json.dumps(
            {
                "value": [
                    {
                        "id": ident,
                        "name": name,
                        "collection": kind,
                        "makingDate": "2021-07-15T00:00:00",
                        "isInForce": True,
                    }
                ]
            }
        )
    )
    register_id = ident if compilation == 0 else f"{ident}C{compilation}"
    (cache / "versions" / f"{ident}.json").write_text(
        json.dumps(
            {
                "registerId": register_id,
                "compilationNumber": str(compilation),
                "start": "2021-07-19T00:00:00",
                "retrospectiveStart": "2021-07-19T00:00:00",
                "hasUnincorporatedAmendments": compilation > 0,
            }
        )
    )
    (cache / "epub" / f"{register_id}-2021-07-19.epub").write_bytes(epub(html))


@pytest.fixture
def setup(tmp_path: Path) -> dict[str, Path]:
    cache = tmp_path / "cache"
    for d in ["titles", "versions", "epub"]:
        (cache / d).mkdir(parents=True)
    names = ["Research Awards Rule 2021 (Latest version)", "Example & Things Act 1991"]
    (cache / "index.html").write_text(index(names))
    register(cache, "Research Awards Rule 2021", "F2021L00996", "Rules", RULE_HTML)
    # ANU writes "&"; the Register only knows the "and" spelling.
    (cache / "titles" / f"{key('Example & Things Act 1991')}.json").write_text(
        json.dumps({"value": []})
    )
    register(cache, "Example and Things Act 1991", "C2004A04206", "Act", ACT_HTML, 15)
    register(cache, "Privacy Act 1988", "C2004A03712", "Act", ACT_HTML)
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
        + ["--delay", "0", "--min-titles", "1", "--cited", "Privacy Act 1988", *extra],
        check=False,
        env=env,
        capture_output=True,
        text=True,
    )


def test_writes_a_record_per_title(setup: dict[str, Path]) -> None:
    result = sync(setup)
    assert result.returncode == 0, result.stderr
    leg = setup["vault"] / "legislation"
    assert sorted(p.name for p in leg.iterdir()) == [
        "example-and-things-act-1991.md",
        "privacy-act-1988.md",
        "research-awards-rule-2021.md",
    ]
    rule = (leg / "research-awards-rule-2021.md").read_text()
    assert (
        "title: Research Awards Rule 2021\nkind: rule\nidentifier: F2021L00996\n"
        in rule
    )
    assert "source: https://www.legislation.gov.au/F2021L00996/latest/text" in rule
    assert "anu_page: https://www.anu.edu.au/about/governance/legislation/l0" in rule
    assert "has_unincorporated_amendments" not in rule
    act = (leg / "example-and-things-act-1991.md").read_text()
    assert "kind: act\n" in act and "compilation: 15\n" in act
    assert "has_unincorporated_amendments: true" in act
    cited = (leg / "privacy-act-1988.md").read_text()
    assert "identifier: C2004A03712\n" in cited and "anu_page" not in cited


def test_headings_and_provisions(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    leg = setup["vault"] / "legislation"
    rule = (leg / "research-awards-rule-2021.md").read_text().split("---\n", 2)[2]
    assert rule.strip().splitlines() == [
        "# Part 3—Program requirements",
        "",
        "### 34 Place where program must be undertaken",
        "",
        "(1) A student must undertake the program at a campus of the University.",
        "",
        "(a) if the program is outside the University;",
        "",
        "(i) supervision is comparable.",
    ]
    act = (leg / "example-and-things-act-1991.md").read_text().split("---\n", 2)[2]
    assert act.strip().splitlines() == [
        "## Part 1—Preliminary",
        "",
        "##### 5 Functions of the University",
        "",
        "(1) The functions include the following:",
        "",
        "(a) advancing knowledge.",
    ]


def test_rerun_writes_nothing_and_drops_stale_records(setup: dict[str, Path]) -> None:
    assert sync(setup).returncode == 0
    stale = setup["vault"] / "legislation" / "repealed-rule-2010.md"
    stale.write_text("old\n")
    result = sync(setup)
    assert result.returncode == 0
    assert "0 files written, 1 removed" in result.stdout
    assert not stale.exists()


def test_refuses_when_a_title_is_missing(setup: dict[str, Path]) -> None:
    names = ["Research Awards Rule 2021", "Nowhere Rule 2020"]
    (setup["cache"] / "index.html").write_text(index(names))
    (setup["cache"] / "titles" / f"{key('Nowhere Rule 2020')}.json").write_text(
        json.dumps({"value": []})
    )
    result = sync(setup)
    assert result.returncode != 0
    assert "Nowhere Rule 2020: not on the Federal Register" in result.stderr
    assert not (setup["vault"] / "legislation").exists()


def test_refuses_a_short_index(setup: dict[str, Path]) -> None:
    result = sync(setup, "--min-titles", "5")
    assert result.returncode != 0
    assert "lists only 2 titles" in result.stdout + result.stderr


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-n", "auto", "-q"]))
