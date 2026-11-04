#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///

from __future__ import annotations

import base64
import os
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parents[1] / "bin" / "rex-claude"

SESSIONS = ["dotfiles", "rex-sessions-tabs", "rex-skill-rename", "Thesis"]

# A stand-in for the rex CLI: every session's focused pane sits in
# $WORK/<label>, which is how the tests see which session was chosen.
FAKE_REX = """#!/bin/sh
case "$1 $2" in
"ls --json")
    printf '{"sessions": [%s]}' "$SESSIONS_JSON" ;;
"session inspect")
    printf '{"session_id": "%s", "active_window_id": "w1", "windows": [
        {"window_id": "w0", "focused_block_id": "b0"},
        {"window_id": "w1", "focused_block_id": "b-%s"}]}' "$3" "$3" ;;
"block call")
    label=${6#b-id-}
    printf '{"last_exit": null, "foreground": {"cwd": "%s/%s"}}' "$WORK" "$label" ;;
*)
    echo "unexpected: $*" >&2; exit 64 ;;
esac
"""


def run(
    tmp_path: Path, *args: str, sessions: list[str] = SESSIONS
) -> subprocess.CompletedProcess[str]:
    fake_bin = tmp_path / "bin"
    fake_bin.mkdir(exist_ok=True)
    rex = fake_bin / "rex"
    rex.write_text(FAKE_REX)
    rex.chmod(0o755)
    sessions_json = ", ".join(
        f'{{"session_id": "id-{label}", "label": "{label}"}}' for label in sessions
    )
    for label in sessions:
        (tmp_path / "work" / label).mkdir(parents=True, exist_ok=True)
    env = os.environ | {
        "PATH": f"{fake_bin}:{os.environ['PATH']}",
        "SESSIONS_JSON": sessions_json,
        "WORK": str(tmp_path / "work"),
    }
    return subprocess.run(
        [SCRIPT, "-n", *args], check=False, capture_output=True, env=env, text=True
    )


def plan(result: subprocess.CompletedProcess[str]) -> dict[str, str]:
    assert result.returncode == 0, result.stderr
    return dict(line.split("\t", 1) for line in result.stdout.splitlines())


@pytest.mark.parametrize(
    ("query", "label"),
    [
        ("dot", "dotfiles"),
        ("rex tabs", "rex-sessions-tabs"),
        ("REX RENAME", "rex-skill-rename"),
        ("thesis", "Thesis"),
    ],
)
def test_keywords_match_in_order_ignoring_case(
    tmp_path: Path, query: str, label: str
) -> None:
    result = plan(run(tmp_path, query))
    assert result["session"] == label
    assert result["dir"] == str(tmp_path / "work" / label)


def test_exact_label_beats_longer_matches(tmp_path: Path) -> None:
    result = run(tmp_path, "dotfiles", sessions=[*SESSIONS, "dotfiles-weddle"])
    assert plan(result)["session"] == "dotfiles"


def test_ambiguous_query_lists_candidates_and_exits_3(tmp_path: Path) -> None:
    result = run(tmp_path, "rex")
    assert result.returncode == 3
    assert "rex-sessions-tabs" in result.stderr
    assert "rex-skill-rename" in result.stderr


def test_keywords_out_of_order_match_nothing(tmp_path: Path) -> None:
    result = run(tmp_path, "tabs rex")
    assert result.returncode == 1
    assert "no session matches" in result.stderr


def test_base64_prompt_survives_quotes_and_dollars(tmp_path: Path) -> None:
    prompt = 'fix Ben\'s $PATH "bug"'
    encoded = base64.b64encode(prompt.encode()).decode()
    result = plan(run(tmp_path, "-b", "dot", encoded))
    assert result["command"].endswith(f"-- {prompt}")
    assert result["command"].startswith(
        "claude --dangerously-skip-permissions --remote-control=dotfiles-"
    )


def test_explicit_dir_skips_the_pane_lookup(tmp_path: Path) -> None:
    elsewhere = str(tmp_path)
    assert plan(run(tmp_path, "-d", elsewhere, "dot"))["dir"] == elsewhere


def test_missing_dir_is_refused(tmp_path: Path) -> None:
    result = run(tmp_path, "-d", str(tmp_path / "missing"), "dot")
    assert result.returncode == 1
    assert "no such directory" in result.stderr


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-v", "-n", "auto"]))
