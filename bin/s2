#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12,<3.14"
# dependencies = ["httpx", "typer"]
# ///
"""CLI for the Semantic Scholar API.

Search papers, get details, list citations/references, and find related work.

Set S2_API_KEY for higher rate limits (optional, free at
https://www.semanticscholar.org/product/api#api-key-form).
"""

import os
import sys
from typing import Annotated, Optional

import httpx
import typer
from httpx import HTTPStatusError

BASE = "https://api.semanticscholar.org/graph/v1"
FIELDS = "title,year,citationCount,authors,venue,externalIds,url"

app = typer.Typer(help="Semantic Scholar CLI")


def client() -> httpx.Client:
    headers = {}
    key = os.environ.get("S2_API_KEY")
    if key:
        headers["x-api-key"] = key
    return httpx.Client(base_url=BASE, headers=headers, timeout=30)


def api_get(c: httpx.Client, path: str, params: dict) -> dict:
    r = c.get(path, params=params)
    try:
        r.raise_for_status()
    except HTTPStatusError:
        if r.status_code == 429:
            print("Rate limited. Set S2_API_KEY for higher limits.", file=sys.stderr)
        else:
            print(f"API error: {r.status_code} {r.text[:200]}", file=sys.stderr)
        raise typer.Exit(1)
    return r.json()


def format_paper(p: dict) -> str:
    authors = ", ".join(a.get("name", "?") for a in (p.get("authors") or [])[:3])
    if len(p.get("authors") or []) > 3:
        authors += " et al."
    year = p.get("year") or "?"
    cites = p.get("citationCount", "?")
    venue = p.get("venue") or ""
    title = p.get("title") or "Untitled"
    paper_id = p.get("paperId", "")
    doi = (p.get("externalIds") or {}).get("DOI", "")

    lines = [f"  {title}", f"  {authors} ({year}) — {cites} citations"]
    if venue:
        lines[1] += f" — {venue}"
    if doi:
        lines.append(f"  doi:{doi}")
    lines.append(f"  https://www.semanticscholar.org/paper/{paper_id}")
    return "\n".join(lines)


@app.command()
def search(
    query: Annotated[str, typer.Argument(help="Search query")],
    limit: Annotated[int, typer.Option(help="Max results")] = 10,
    year: Annotated[Optional[str], typer.Option(help="Year or range, e.g. 2020 or 2020-2024")] = None,
    open_access: Annotated[bool, typer.Option("--oa", help="Open access only")] = False,
):
    """Search for papers."""
    params: dict = {"query": query, "limit": limit, "fields": FIELDS}
    if year:
        params["year"] = year
    if open_access:
        params["openAccessPdf"] = ""
    with client() as c:
        data = api_get(c, "/paper/search", params).get("data", [])
    if not data:
        print("No results.")
        raise typer.Exit()
    for i, p in enumerate(data, 1):
        print(f"{i}. {format_paper(p)}\n")


@app.command()
def paper(
    paper_id: Annotated[str, typer.Argument(help="Paper ID, DOI, arXiv ID, or S2 URL")],
):
    """Get details for a specific paper."""
    with client() as c:
        p = api_get(c, f"/paper/{paper_id}", {"fields": FIELDS + ",abstract,tldr"})
    print(format_paper(p))
    tldr = (p.get("tldr") or {}).get("text")
    if tldr:
        print(f"\n  tl;dr: {tldr}")
    abstract = p.get("abstract")
    if abstract:
        print(f"\n  Abstract: {abstract}")


@app.command()
def citations(
    paper_id: Annotated[str, typer.Argument(help="Paper ID, DOI, arXiv ID, or S2 URL")],
    limit: Annotated[int, typer.Option(help="Max results")] = 10,
):
    """List papers that cite this paper."""
    with client() as c:
        data = api_get(c, f"/paper/{paper_id}/citations", {"fields": FIELDS, "limit": limit}).get("data", [])
    if not data:
        print("No citations found.")
        raise typer.Exit()
    for i, entry in enumerate(data, 1):
        p = entry.get("citingPaper", {})
        print(f"{i}. {format_paper(p)}\n")


@app.command()
def references(
    paper_id: Annotated[str, typer.Argument(help="Paper ID, DOI, arXiv ID, or S2 URL")],
    limit: Annotated[int, typer.Option(help="Max results")] = 10,
):
    """List papers referenced by this paper."""
    with client() as c:
        data = api_get(c, f"/paper/{paper_id}/references", {"fields": FIELDS, "limit": limit}).get("data", [])
    if not data:
        print("No references found.")
        raise typer.Exit()
    for i, entry in enumerate(data, 1):
        p = entry.get("citedPaper", {})
        print(f"{i}. {format_paper(p)}\n")


@app.command()
def related(
    paper_id: Annotated[str, typer.Argument(help="Paper ID, DOI, arXiv ID, or S2 URL")],
):
    """Find papers recommended based on this paper."""
    with client() as c:
        data = api_get(c, f"/recommendations/v1/papers/forpaper/{paper_id}", {"fields": FIELDS, "limit": 10}).get("recommendedPapers", [])
    if not data:
        print("No recommendations found.")
        raise typer.Exit()
    for i, p in enumerate(data, 1):
        print(f"{i}. {format_paper(p)}\n")


if __name__ == "__main__":
    app()
