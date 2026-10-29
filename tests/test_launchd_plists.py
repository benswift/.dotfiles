"""Every tracked launchd plist is well-formed XML.

An XML comment may not contain `--`, so the `---` dash the rest of the repo
uses breaks a plist's header comment; plutil and plistlib then reject it.
"""

import plistlib
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
PLISTS = sorted([*REPO.glob("launchd/*.plist"), *REPO.glob("private/launchd/*.plist")])


@pytest.mark.parametrize("plist", PLISTS, ids=lambda p: p.name)
def test_plist_parses(plist: Path) -> None:
    with plist.open("rb") as f:
        assert plistlib.load(f)["Label"] == plist.stem
