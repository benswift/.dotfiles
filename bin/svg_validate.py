#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12,<3.14"
# dependencies = ["lxml>=5.0", "typer>=0.12"]
# ///
"""Validate SVG illustrations: well-formedness + design invariants.

Pretty-prints with `--fix`. Designed for the ben:svg-gen skill.
"""

from __future__ import annotations

import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Annotated, Literal

import typer
from lxml import etree

Level = Literal["ok", "warn", "err"]


@dataclass(frozen=True)
class CheckResult:
    level: Level
    check: str
    message: str


SVG_NS = "http://www.w3.org/2000/svg"
XLINK_NS = "http://www.w3.org/1999/xlink"


def parse_svg(text: str) -> tuple[etree._Element | None, CheckResult]:
    parser = etree.XMLParser(remove_blank_text=False, resolve_entities=False)
    try:
        root = etree.fromstring(text.encode("utf-8"), parser)
    except etree.XMLSyntaxError as e:
        return None, CheckResult("err", "xml", f"not well-formed: {e}")
    return root, CheckResult("ok", "xml", "well-formed XML")


def _local_name(element: etree._Element) -> str:
    tag = element.tag
    if isinstance(tag, str) and "}" in tag:
        return tag.split("}", 1)[1]
    return str(tag)


def check_root_svg(root: etree._Element) -> CheckResult:
    name = _local_name(root)
    if name != "svg":
        return CheckResult("err", "root", f"root element is <{name}>, not <svg>")
    return CheckResult("ok", "root", f"root is <svg>")


def check_viewbox(root: etree._Element) -> CheckResult:
    vb = root.get("viewBox")
    if not vb:
        return CheckResult("err", "viewbox", "<svg> has no viewBox attribute")
    return CheckResult("ok", "viewbox", f'viewBox="{vb}"')


app = typer.Typer(add_completion=False)


@app.command()
def main(path: Annotated[Path, typer.Argument(exists=True, dir_okay=False)]) -> None:
    text = path.read_text(encoding="utf-8")
    root, result = parse_svg(text)
    _print_result(result)
    if root is None:
        raise typer.Exit(1)

    results = [check_root_svg(root), check_viewbox(root)]
    for r in results:
        _print_result(r)

    exit_code = 1 if any(r.level == "err" for r in results) else 0
    raise typer.Exit(exit_code)


def _print_result(r: CheckResult) -> None:
    glyph = {"ok": "✓", "warn": "⚠", "err": "✗"}[r.level]
    print(f"{glyph} {r.check}: {r.message}")


if __name__ == "__main__":
    app()
