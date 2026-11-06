#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run the agy-project-context SessionStart hook against a temporary repo."""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "agy-project-context"


def run(*workspaces: Path) -> dict:
    payload = {"conversationId": "c", "workspacePaths": [str(w) for w in workspaces]}
    out = subprocess.run(
        [str(SCRIPT)],
        input=json.dumps(payload),
        capture_output=True,
        text=True,
        check=True,
    )
    return json.loads(out.stdout)


def message(output: dict) -> str:
    return output["injectSteps"][0]["systemMessage"]["systemMessage"]


@pytest.fixture
def repo(tmp_path: Path) -> Path:
    root = tmp_path / "repo"
    (root / "pkg" / "sub").mkdir(parents=True)
    subprocess.run(["git", "init", "-q", str(root)], check=True)
    (tmp_path / "CLAUDE.md").write_text("above the repo")
    (root / "CLAUDE.md").write_text("root rules")
    (root / "pkg" / "CLAUDE.md").write_text("pkg rules")
    return root


def test_collects_root_to_workspace_in_order(repo: Path) -> None:
    text = message(run(repo / "pkg" / "sub"))
    assert text.index("root rules") < text.index("pkg rules")
    assert "above the repo" not in text


def test_skips_a_directory_agy_already_reads(repo: Path) -> None:
    (repo / "pkg" / "AGENTS.md").write_text("agy reads this itself")
    text = message(run(repo / "pkg"))
    assert "root rules" in text
    assert "pkg rules" not in text


def test_nothing_to_add_is_an_empty_object(tmp_path: Path) -> None:
    assert run(tmp_path, tmp_path / "missing") == {}


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-n", "auto", "-q"]))
