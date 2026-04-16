#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist", "lxml>=5.0", "typer>=0.12"]
# ///

import importlib.machinery
import importlib.util
import subprocess
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


SCRIPT = Path(__file__).parent.parent / "bin" / "svg_validate.py"

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


parse_hex = mod.parse_hex
srgb_to_lab = mod.srgb_to_lab
delta_e_76 = mod.delta_e_76


class TestPaletteHelpers:
    def test_parse_hex_long_form(self):
        assert parse_hex("#b58900") == (0xb5, 0x89, 0x00)

    def test_parse_hex_short_form(self):
        assert parse_hex("#abc") == (0xaa, 0xbb, 0xcc)

    def test_parse_hex_no_hash(self):
        assert parse_hex("b58900") == (0xb5, 0x89, 0x00)

    def test_parse_hex_invalid_returns_none(self):
        assert parse_hex("not-a-colour") is None
        assert parse_hex("red") is None  # named colours unsupported here

    def test_lab_identity(self):
        l1, a1, b1 = srgb_to_lab((255, 0, 0))
        l2, a2, b2 = srgb_to_lab((255, 0, 0))
        assert (l1, a1, b1) == (l2, a2, b2)

    def test_delta_e_identical_is_zero(self):
        c = srgb_to_lab((181, 137, 0))
        assert delta_e_76(c, c) == 0.0

    def test_delta_e_similar_small(self):
        # Two near-identical solarized yellows.
        c1 = srgb_to_lab((181, 137, 0))     # #b58900
        c2 = srgb_to_lab((183, 138, 2))
        assert delta_e_76(c1, c2) < 3.0

    def test_delta_e_distant_large(self):
        c1 = srgb_to_lab((181, 137, 0))
        c2 = srgb_to_lab((38, 139, 210))    # #268bd2 (blue)
        assert delta_e_76(c1, c2) > 30.0


check_palette = mod.check_palette


SOLARIZED_WARM = [(181, 137, 0), (203, 75, 22), (220, 50, 47)]  # #b58900 #cb4b16 #dc322f


class TestCheckPalette:
    def test_empty_palette_passes(self):
        root, _ = parse_svg(VALID_SVG)
        result = check_palette(root, [])
        assert result.level == "ok"
        assert "no palette" in result.message.lower()

    def test_colours_in_palette_pass(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><path d="M0,0" fill="#b58900"/><path d="M1,1" stroke="#cb4b16"/></svg>'
        root, _ = parse_svg(svg)
        result = check_palette(root, SOLARIZED_WARM)
        assert result.level == "ok"

    def test_off_palette_colour_warns(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><path d="M0,0" fill="#00ff00"/></svg>'
        root, _ = parse_svg(svg)
        result = check_palette(root, SOLARIZED_WARM)
        assert result.level == "warn"
        assert "#00ff00" in result.message.lower() or "off-palette" in result.message.lower()

    def test_none_and_currentcolor_pass(self):
        svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><path fill="none" stroke="currentColor"/></svg>'
        root, _ = parse_svg(svg)
        result = check_palette(root, SOLARIZED_WARM)
        assert result.level == "ok"

    def test_url_gradient_ref_pass_but_stops_checked(self):
        svg = (
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10">'
            '<defs><linearGradient id="g"><stop offset="0" stop-color="#b58900"/>'
            '<stop offset="1" stop-color="#cb4b16"/></linearGradient></defs>'
            '<rect width="10" height="10" fill="url(#g)"/></svg>'
        )
        root, _ = parse_svg(svg)
        result = check_palette(root, SOLARIZED_WARM)
        assert result.level == "ok"


pretty_print = mod.pretty_print


class TestPrettyPrint:
    def test_uses_2_space_indent(self):
        root, _ = parse_svg('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><g><rect width="1" height="1"/></g></svg>')
        out = pretty_print(root)
        # Root is unindented; first child is 2 spaces; grandchild is 4.
        assert "\n  <g" in out
        assert "\n    <rect" in out

    def test_round_trip_preserves_viewbox(self):
        root, _ = parse_svg(VALID_SVG)
        out = pretty_print(root)
        assert 'viewBox="0 0 100 100"' in out

    def test_no_xml_declaration(self):
        root, _ = parse_svg(VALID_SVG)
        out = pretty_print(root)
        assert not out.startswith("<?xml")


class TestStrictFlag:
    def test_strict_promotes_warning_to_exit_1(self, tmp_path: Path):
        p = tmp_path / "dup.svg"
        p.write_text(
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10">'
            '<path d="M0,0" fill="#aaa"/><path d="M0,0" fill="#aaa"/></svg>'
        )
        # Default exits 0 despite warnings.
        r1 = subprocess.run([str(SCRIPT), str(p)], capture_output=True)
        assert r1.returncode == 0
        # --strict exits 1.
        r2 = subprocess.run([str(SCRIPT), str(p), "--strict"], capture_output=True)
        assert r2.returncode == 1


if __name__ == "__main__":
    import pytest
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
