#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Feed bin/claude-git-guard hook payloads and check which it blocks."""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "claude-git-guard"


def run(command: str) -> subprocess.CompletedProcess[str]:
    payload = json.dumps({"tool_name": "Bash", "tool_input": {"command": command}})
    return subprocess.run(
        [str(SCRIPT)], input=payload, capture_output=True, text=True, check=False
    )


@pytest.mark.parametrize(
    "command",
    [
        "git push --force",
        "git push -f origin main",
        "git push origin main --force",
        "git -C ~/repo push --force",
        "cd repo && git push --force",
        "git add .",
        "git add -A",
        "git add --all",
        "git -C private add .",
        "git add . && git commit -m wip",
    ],
)
def test_blocks(command: str) -> None:
    result = run(command)
    assert result.returncode == 2
    assert "claude-git-guard" in result.stderr


@pytest.mark.parametrize(
    "command",
    [
        "git push",
        "git push --force-with-lease",
        "git push --force-with-lease origin main",
        "git add bin/foo tests/test_foo.py",
        "git add ./bin/foo",
        "git add -p",
        "git status && git diff --stat",
        "ls -f",
        "rg 'git add' .",
        "",
    ],
)
def test_allows(command: str) -> None:
    result = run(command)
    assert result.returncode == 0, result.stderr


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-q", "-n", "auto"]))
