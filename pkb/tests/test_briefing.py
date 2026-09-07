from __future__ import annotations

from pathlib import Path

from pkb_tools import briefing


def test_sections_are_replaced_in_place_and_ordered(notebook: Path) -> None:
    briefing.write_section("health", "all fine")
    briefing.write_section("mail", "one draft")
    briefing.write_section("health", "one problem")
    text = (notebook / "briefing.md").read_text()
    assert text.startswith("# Briefing")
    assert text.count("<!-- pkb:health -->") == 1
    assert "all fine" not in text
    assert text.index("pkb:mail") < text.index("pkb:health")
    assert briefing.read_section(
        "health",
    ).startswith("one problem")  # type: ignore[union-attr]


def test_unknown_sections_survive_a_rewrite(notebook: Path) -> None:
    briefing.write_section("custom", "kept")
    briefing.write_section("mail", "x")
    assert briefing.read_section(
        "custom",
    ).startswith("kept")  # type: ignore[union-attr]
