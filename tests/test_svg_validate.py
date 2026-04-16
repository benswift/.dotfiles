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


if __name__ == "__main__":
    import pytest
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
