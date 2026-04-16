#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12,<3.14"
# dependencies = ["lxml>=5.0", "typer>=0.12"]
# ///
"""Validate SVG illustrations: well-formedness + design invariants.

Pretty-prints with `--fix`. Designed for the ben:svg-gen skill.
"""

from __future__ import annotations

import math
import re
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


_HEX_RE = re.compile(r"^#?([0-9a-fA-F]{3}|[0-9a-fA-F]{6})$")


def parse_hex(s: str) -> tuple[int, int, int] | None:
    m = _HEX_RE.match(s.strip())
    if not m:
        return None
    hx = m.group(1)
    if len(hx) == 3:
        r, g, b = (int(c * 2, 16) for c in hx)
    else:
        r, g, b = (int(hx[i : i + 2], 16) for i in (0, 2, 4))
    return (r, g, b)


def _srgb_to_linear(c: float) -> float:
    c = c / 255.0
    return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4


def _f_lab(t: float) -> float:
    return t ** (1 / 3) if t > 0.008856 else (7.787 * t + 16 / 116)


def srgb_to_lab(rgb: tuple[int, int, int]) -> tuple[float, float, float]:
    r, g, b = (_srgb_to_linear(v) for v in rgb)
    # sRGB D65 -> XYZ
    x = 0.4124564 * r + 0.3575761 * g + 0.1804375 * b
    y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b
    z = 0.0193339 * r + 0.1191920 * g + 0.9503041 * b
    # Normalise to D65 reference white
    xr, yr, zr = x / 0.95047, y / 1.0, z / 1.08883
    fx, fy, fz = _f_lab(xr), _f_lab(yr), _f_lab(zr)
    L = 116 * fy - 16
    a = 500 * (fx - fy)
    b_ = 200 * (fy - fz)
    return (L, a, b_)


def delta_e_76(lab1: tuple[float, float, float], lab2: tuple[float, float, float]) -> float:
    return math.sqrt(sum((a - b) ** 2 for a, b in zip(lab1, lab2)))


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


def check_no_script(root: etree._Element) -> CheckResult:
    scripts = root.xpath('.//*[local-name()="script"]')
    if scripts:
        return CheckResult("err", "script", f"{len(scripts)} <script> element(s) present")
    return CheckResult("ok", "script", "no <script> elements")


DANGEROUS_HREF_PREFIXES = (
    "javascript:",
    "data:text/html",
    "data:text/javascript",
    "data:application/javascript",
    "vbscript:",
)


def _iter_href_values(root: etree._Element):
    for el in root.iter():
        for attr in ("href", f"{{{XLINK_NS}}}href"):
            v = el.get(attr)
            if v:
                yield el, v


def check_dangerous_hrefs(root: etree._Element) -> CheckResult:
    bad: list[str] = []
    for _el, value in _iter_href_values(root):
        lowered = value.strip().lower()
        for prefix in DANGEROUS_HREF_PREFIXES:
            if lowered.startswith(prefix):
                bad.append(value[:60])
                break
    if bad:
        joined = "; ".join(bad)
        return CheckResult("err", "href", f"dangerous href(s): {joined}")
    return CheckResult("ok", "href", "no dangerous href values")


EXTERNAL_HREF_PREFIXES = ("http://", "https://", "file://", "ftp://")


def check_external_hrefs(root: etree._Element) -> CheckResult:
    external: list[str] = []
    for _el, value in _iter_href_values(root):
        lowered = value.strip().lower()
        for prefix in EXTERNAL_HREF_PREFIXES:
            if lowered.startswith(prefix):
                external.append(value[:60])
                break
    if external:
        joined = "; ".join(external)
        return CheckResult("warn", "external-href", f"external href(s): {joined}")
    return CheckResult("ok", "external-href", "no external href values")


def check_embedded_raster(root: etree._Element) -> CheckResult:
    images = root.xpath('.//*[local-name()="image"]')
    if images:
        return CheckResult("warn", "raster", f"{len(images)} embedded <image> element(s)")
    return CheckResult("ok", "raster", "no embedded raster images")


def check_node_count(root: etree._Element, limit: int) -> CheckResult:
    count = sum(1 for _ in root.iter())
    msg = f"{count} elements (limit {limit})"
    if count > limit:
        return CheckResult("warn", "nodes", msg)
    return CheckResult("ok", "nodes", msg)


def check_duplicate_paths(root: etree._Element) -> CheckResult:
    paths = root.xpath('.//*[local-name()="path"]')
    seen: dict[tuple, int] = {}
    for p in paths:
        key = (
            p.get("d") or "",
            p.get("fill") or "",
            p.get("stroke") or "",
            p.get("stroke-width") or "",
        )
        seen[key] = seen.get(key, 0) + 1
    duplicates = sum(count - 1 for count in seen.values() if count > 1)
    if duplicates:
        return CheckResult("warn", "duplicates", f"{duplicates} duplicate <path> element(s)")
    return CheckResult("ok", "duplicates", "no duplicate <path> elements")


_COLOUR_ATTRS = ("fill", "stroke", "stop-color", "flood-color", "lighting-color")


def _iter_colour_values(root: etree._Element):
    for el in root.iter():
        for attr in _COLOUR_ATTRS:
            v = el.get(attr)
            if v:
                yield el, attr, v


DELTA_E_THRESHOLD = 3.0


def check_palette(root: etree._Element, palette_rgb: list[tuple[int, int, int]]) -> CheckResult:
    if not palette_rgb:
        return CheckResult("ok", "palette", "no palette declared; skipping")

    palette_lab = [srgb_to_lab(c) for c in palette_rgb]
    off: list[str] = []
    for _el, _attr, raw in _iter_colour_values(root):
        v = raw.strip().lower()
        if v in ("none", "currentcolor", "inherit", "transparent"):
            continue
        if v.startswith("url(") or v.startswith("var("):
            continue
        rgb = parse_hex(v)
        if rgb is None:
            # Named colours / rgb()/hsl() left unchecked for now.
            continue
        lab = srgb_to_lab(rgb)
        if not any(delta_e_76(lab, p) <= DELTA_E_THRESHOLD for p in palette_lab):
            off.append(v)

    uniq_off = sorted(set(off))
    if uniq_off:
        joined = ", ".join(uniq_off[:5]) + (", …" if len(uniq_off) > 5 else "")
        return CheckResult("warn", "palette", f"off-palette colour(s): {joined}")
    return CheckResult("ok", "palette", f"all colours within {len(palette_rgb)}-colour palette")


def pretty_print(root: etree._Element) -> str:
    tree = etree.ElementTree(root)
    # Strip then re-apply whitespace so lxml re-indents cleanly.
    for el in root.iter():
        if el.text is not None and not el.text.strip():
            el.text = None
        if el.tail is not None and not el.tail.strip():
            el.tail = None
    etree.indent(tree, space="  ")
    return etree.tostring(tree, pretty_print=True, encoding="unicode")


def _checks_on_root(
    root: etree._Element,
    palette_rgb: list[tuple[int, int, int]] | None,
    max_nodes: int,
) -> list[CheckResult]:
    return [
        check_root_svg(root),
        check_viewbox(root),
        check_no_script(root),
        check_dangerous_hrefs(root),
        check_external_hrefs(root),
        check_embedded_raster(root),
        check_node_count(root, max_nodes),
        check_duplicate_paths(root),
        check_palette(root, palette_rgb or []),
    ]


def run_checks(
    text: str,
    palette_rgb: list[tuple[int, int, int]] | None = None,
    max_nodes: int = 500,
) -> list[CheckResult]:
    root, parse_result = parse_svg(text)
    if root is None:
        return [parse_result]
    return [parse_result] + _checks_on_root(root, palette_rgb, max_nodes)


app = typer.Typer(add_completion=False)


@app.command()
def main(
    path: Annotated[Path, typer.Argument(exists=True, dir_okay=False)],
    max_nodes: Annotated[int, typer.Option("--max-nodes")] = 500,
    palette: Annotated[str, typer.Option("--palette", help="Comma- or space-separated hex colours")] = "",
    fix: Annotated[bool, typer.Option("--fix", help="Pretty-print the file in place")] = False,
    strict: Annotated[bool, typer.Option("--strict", help="Treat warnings as errors")] = False,
) -> None:
    text = path.read_text(encoding="utf-8")

    palette_rgb: list[tuple[int, int, int]] = []
    if palette.strip():
        for token in re.split(r"[,\s]+", palette.strip()):
            rgb = parse_hex(token)
            if rgb is None:
                print(f"warning: ignoring unparseable palette token: {token}", file=sys.stderr)
            else:
                palette_rgb.append(rgb)

    root, parse_result = parse_svg(text)
    _print_result(parse_result)
    if root is None:
        raise typer.Exit(1)

    results = [parse_result] + _checks_on_root(root, palette_rgb, max_nodes)
    for r in results[1:]:
        _print_result(r)

    if fix:
        path.write_text(pretty_print(root), encoding="utf-8")
        print("formatted: 2-space indent")

    has_err = any(r.level == "err" for r in results)
    has_warn = any(r.level == "warn" for r in results)
    exit_code = 1 if has_err or (strict and has_warn) else 0
    raise typer.Exit(exit_code)


def _print_result(r: CheckResult) -> None:
    glyph = {"ok": "✓", "warn": "⚠", "err": "✗"}[r.level]
    stream = sys.stdout if r.level == "ok" else sys.stderr
    print(f"{glyph} {r.check}: {r.message}", file=stream)


if __name__ == "__main__":
    app()
