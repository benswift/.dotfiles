#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist", "lxml>=5.0", "typer>=0.12"]
# ///

import importlib.machinery
import importlib.util
import sys
from pathlib import Path

_script_path = str(Path(__file__).parent.parent / "bin" / "svg_validate.py")
_loader = importlib.machinery.SourceFileLoader("svg_validate", _script_path)
spec = importlib.util.spec_from_loader("svg_validate", _loader, origin=_script_path)
mod = importlib.util.module_from_spec(spec)
sys.modules["svg_validate"] = mod
spec.loader.exec_module(mod)

parse_svg = mod.parse_svg
CheckResult = mod.CheckResult


VALID_SVG = """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
  <path d="M10,50 C30,10 70,10 90,50" fill="#b58900" stroke="none"/>
</svg>"""


class TestParseSvg:
    def test_valid_svg_parses(self):
        root, result = parse_svg(VALID_SVG)
        assert root is not None
        assert result.level == "ok"
        assert result.check == "xml"

    def test_malformed_xml_errors(self):
        root, result = parse_svg("<svg><unclosed>")
        assert root is None
        assert result.level == "err"
        assert result.check == "xml"
        assert "not well-formed" in result.message


check_root_svg = mod.check_root_svg


class TestCheckRootSvg:
    def test_svg_root_passes(self):
        root, _ = parse_svg(VALID_SVG)
        result = check_root_svg(root)
        assert result.level == "ok"
        assert result.check == "root"

    def test_non_svg_root_errors(self):
        root, _ = parse_svg('<html><body/></html>')
        result = check_root_svg(root)
        assert result.level == "err"
        assert "html" in result.message.lower()


check_viewbox = mod.check_viewbox


class TestCheckViewbox:
    def test_present_viewbox_passes(self):
        root, _ = parse_svg(VALID_SVG)
        result = check_viewbox(root)
        assert result.level == "ok"
        assert "0 0 100 100" in result.message

    def test_missing_viewbox_errors(self):
        root, _ = parse_svg('<svg xmlns="http://www.w3.org/2000/svg"/>')
        result = check_viewbox(root)
        assert result.level == "err"
        assert "viewbox" in result.message.lower()


check_no_script = mod.check_no_script


class TestCheckNoScript:
    def test_no_script_passes(self):
        root, _ = parse_svg(VALID_SVG)
        result = check_no_script(root)
        assert result.level == "ok"

    def test_script_element_errors(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><script>alert(1)</script></svg>'
        root, _ = parse_svg(svg)
        result = check_no_script(root)
        assert result.level == "err"
        assert "script" in result.message.lower()


check_dangerous_hrefs = mod.check_dangerous_hrefs


class TestCheckDangerousHrefs:
    def test_clean_svg_passes(self):
        root, _ = parse_svg(VALID_SVG)
        result = check_dangerous_hrefs(root)
        assert result.level == "ok"

    def test_javascript_href_errors(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 10 10"><a xlink:href="javascript:alert(1)"><rect/></a></svg>'
        root, _ = parse_svg(svg)
        result = check_dangerous_hrefs(root)
        assert result.level == "err"

    def test_data_text_html_errors(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><use href="data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg=="/></svg>'
        root, _ = parse_svg(svg)
        result = check_dangerous_hrefs(root)
        assert result.level == "err"


check_external_hrefs = mod.check_external_hrefs


class TestCheckExternalHrefs:
    def test_no_external_hrefs_passes(self):
        root, _ = parse_svg(VALID_SVG)
        result = check_external_hrefs(root)
        assert result.level == "ok"

    def test_https_href_warns(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><image href="https://example.com/x.png" width="10" height="10"/></svg>'
        root, _ = parse_svg(svg)
        result = check_external_hrefs(root)
        assert result.level == "warn"

    def test_internal_fragment_passes(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><defs><linearGradient id="g"/></defs><rect fill="url(#g)"/></svg>'
        root, _ = parse_svg(svg)
        result = check_external_hrefs(root)
        assert result.level == "ok"


check_embedded_raster = mod.check_embedded_raster


class TestCheckEmbeddedRaster:
    def test_no_image_passes(self):
        root, _ = parse_svg(VALID_SVG)
        result = check_embedded_raster(root)
        assert result.level == "ok"

    def test_image_element_warns(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><image href="data:image/png;base64,iVBORw0KGgo=" width="10" height="10"/></svg>'
        root, _ = parse_svg(svg)
        result = check_embedded_raster(root)
        assert result.level == "warn"
        assert "image" in result.message.lower()


check_node_count = mod.check_node_count


class TestCheckNodeCount:
    def test_below_limit_passes(self):
        root, _ = parse_svg(VALID_SVG)
        result = check_node_count(root, limit=500)
        assert result.level == "ok"
        assert "limit 500" in result.message

    def test_above_limit_warns(self):
        paths = "".join(f'<rect x="{i}" y="0" width="1" height="1"/>' for i in range(50))
        svg = f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 50 1">{paths}</svg>'
        root, _ = parse_svg(svg)
        result = check_node_count(root, limit=10)
        assert result.level == "warn"


check_duplicate_paths = mod.check_duplicate_paths


class TestCheckDuplicatePaths:
    def test_unique_paths_pass(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><path d="M0,0 L1,1" fill="#aaa"/><path d="M2,2 L3,3" fill="#bbb"/></svg>'
        root, _ = parse_svg(svg)
        result = check_duplicate_paths(root)
        assert result.level == "ok"

    def test_duplicate_paths_warn(self):
        p = '<path d="M0,0 L1,1" fill="#aaa" stroke="none"/>'
        svg = f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10">{p}{p}{p}</svg>'
        root, _ = parse_svg(svg)
        result = check_duplicate_paths(root)
        assert result.level == "warn"
        assert "duplicate" in result.message.lower()


if __name__ == "__main__":
    import pytest
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
