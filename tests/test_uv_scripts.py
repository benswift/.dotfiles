"""Every `uv run --script` tool in bin/ is linted and locked.

ruff.toml has to list each one by name, and each needs a committed lock;
a tool missing from either is silently unlinted or unpinned.
"""

import tomllib
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent


def uv_scripts() -> list[str]:
    return sorted(
        f"bin/{p.name}"
        for p in (REPO / "bin").iterdir()
        if p.is_file()
        and not p.name.endswith(".lock")
        and p.read_bytes().startswith(b"#!/usr/bin/env -S uv run")
    )


def test_found_some() -> None:
    assert len(uv_scripts()) > 5


def test_every_uv_script_is_linted() -> None:
    config = tomllib.loads((REPO / "ruff.toml").read_text())
    missing = set(uv_scripts()) - set(config["extend-include"])
    assert not missing, f"add to extend-include in ruff.toml: {sorted(missing)}"


def test_every_uv_script_is_locked() -> None:
    missing = [s for s in uv_scripts() if not (REPO / f"{s}.lock").is_file()]
    assert not missing, f"run `uv lock --script` for: {missing}"
