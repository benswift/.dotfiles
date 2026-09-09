from __future__ import annotations

import shutil
from pathlib import Path

import pytest
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


def test_written_file_is_an_oxfmt_fixed_point(notebook: Path) -> None:
    """Ben edits this file, and Helix formats markdown on save."""
    if shutil.which("oxfmt-helix") is None:
        pytest.skip("oxfmt-helix not on PATH")
    body = (
        "## Needs a reply (1)\n\n"
        "1. **Ada Lovelace** \u00b7 anu \u00b7 Mon 7 Sep (today) \u00b7 [[people/ada-lovelace]]  \n"
        "   Workshop? --- Ada asks whether Ben can run the analytical engine workshop in the second half of semester.  \n"
        "   Draft:\n\n"
        "   > Hi Ada,\n   >\n   > Yes, [date] suits.\n\n"
        "   `mail-compose -f anu --reply-to '/x' --body - --send`\n\n"
        "   <!-- pkb:note a@b.c --- hint -->\n\n"
        "   Say yes, and ask her to pick a week after the mid-semester break.\n"
    )
    briefing.write_section("mail", body)
    path = notebook / briefing.FILENAME
    written = path.read_text()
    assert briefing.formatted(written) == written
    # the send command must not have been swallowed by the draft blockquote
    assert "\n   `mail-compose" in written
