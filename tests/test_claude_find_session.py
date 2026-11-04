#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Ship sessions into a temporary HOME, index them with the real ingester, and
search them with claude-find-session."""

from __future__ import annotations

import json
import os
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

import pytest

BIN = Path(__file__).parent.parent / "bin"
NOW = time.time()


def stamp(days_ago: float) -> str:
    return time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime(NOW - days_ago * 86400))


def user(content: Any, **extra: Any) -> dict[str, Any]:
    return {"type": "user", "message": {"role": "user", "content": content}} | extra


def ship_claude(
    home: Path,
    uuid: str,
    records: list[dict[str, Any]],
    *,
    host: str = "weddle",
    cwd: str = "/home/ben/projects/jane",
    days_ago: float = 1,
    entrypoint: str = "cli",
) -> None:
    project = home / "claude-logs" / host / cwd.replace("/", "-")
    project.mkdir(parents=True, exist_ok=True)
    base = {"cwd": cwd, "entrypoint": entrypoint, "timestamp": stamp(days_ago)}
    lines = [json.dumps(base | record) for record in records]
    (project / f"{uuid}.jsonl").write_text("\n".join(lines) + "\n")
    (home / "claude-logs" / host / ".last-ship").write_text(f"{int(NOW)}\n")


def ship_codex(home: Path, uuid: str, prompt: str) -> None:
    shard = home / "codex-logs" / "weddle" / "2026" / "10" / "30"
    shard.mkdir(parents=True, exist_ok=True)
    records = [
        {
            "type": "session_meta",
            "payload": {
                "timestamp": stamp(3),
                "cwd": "/home/ben/.dotfiles",
                "originator": "codex-tui",
            },
        },
        {
            "type": "response_item",
            "payload": {
                "type": "message",
                "role": "user",
                "content": [
                    {"type": "input_text", "text": "# AGENTS.md instructions for x"},
                    {"type": "input_text", "text": prompt},
                ],
            },
        },
    ]
    name = f"rollout-2026-10-30T09-00-00-{uuid}.jsonl"
    (shard / name).write_text("\n".join(map(json.dumps, records)) + "\n")


@pytest.fixture
def home(tmp_path: Path) -> Path:
    ship_claude(
        tmp_path,
        "jane-fly",
        [
            user("is our fly machine under-provisioned?"),
            {"type": "ai-title", "aiTitle": "Fly machine sizing"},
            user([{"type": "tool_result", "content": "memory 512mb"}]),
            user("ok, bump the memory then"),
        ],
        days_ago=5,
    )
    ship_claude(
        tmp_path,
        "daysy-cv",
        [user("variant of my cv for the ANAT grant")],
        host="daysy",
        cwd="/Users/ben/.dotfiles",
        days_ago=2,
    )
    ship_claude(
        tmp_path,
        "headless",
        [user("the fly machine memory report")],
        entrypoint="sdk-cli",
    )
    ship_claude(
        tmp_path,
        "harness-only",
        [
            user("hello"),
            user(
                [
                    {
                        "type": "text",
                        "text": "<system-reminder>fly memory</system-reminder>",
                    }
                ]
            ),
            user("fly memory skill text", isMeta=True),
            user("summary: fly memory", isCompactSummary=True),
        ],
    )
    ship_claude(
        tmp_path,
        "slash",
        [
            user(
                "<command-name>/ben:vault</command-name><command-args>grant deadlines</command-args>"
            )
        ],
        cwd="/home/ben/.dotfiles",
    )
    ship_codex(
        tmp_path, "019feb7a-26bc-7381-bb87-bb3928675607", "fix the helix grammar"
    )
    indexed = subprocess.run(
        [sys.executable, str(BIN / "ingest-claude-logs")],
        env={**os.environ, "HOME": str(tmp_path)},
        capture_output=True,
        text=True,
        check=False,
    )
    assert indexed.returncode == 0, indexed.stderr
    return tmp_path


def find(home: Path, *args: str, session: str = "") -> subprocess.CompletedProcess[str]:
    env = {**os.environ, "HOME": str(home), "CLAUDE_CODE_SESSION_ID": session}
    return subprocess.run(
        [sys.executable, str(BIN / "claude-find-session"), "--from", "weddle", *args],
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def test_terms_may_match_different_prompts(home: Path) -> None:
    result = find(home, "fly", "memory")
    assert result.returncode == 0, result.stderr
    header, *rest = result.stdout.splitlines()
    assert header.endswith("weddle  ~/projects/jane  Fly machine sizing")
    assert rest == [
        "  > is our fly machine under-provisioned?",
        "  > ok, bump the memory then",
        "  cd /home/ben/projects/jane && claude --resume jane-fly",
    ]


def test_headless_and_harness_text_are_not_searched(home: Path) -> None:
    result = find(home, "fly", "memory")
    assert "headless" not in result.stdout
    assert "harness-only" not in result.stdout


def test_tool_results_are_not_prompts(home: Path) -> None:
    result = find(home, "512mb")
    assert result.returncode == 1
    assert "no matching session" in result.stderr


def test_session_on_another_host_resumes_over_ssh(home: Path) -> None:
    result = find(home, "anat")
    assert "  ssh -t daysy 'cd /Users/ben/.dotfiles && claude --resume daysy-cv'" in (
        result.stdout.splitlines()
    )


def test_slash_command_arguments_are_searched(home: Path) -> None:
    result = find(home, "grant")
    assert "  > /ben:vault grant deadlines" in result.stdout.splitlines()
    assert "daysy-cv" in result.stdout


def test_newest_first_and_limit(home: Path) -> None:
    result = find(home, "grant", "-n", "1")
    assert "claude --resume slash" in result.stdout
    assert "daysy-cv" not in result.stdout


def test_filters_by_project_and_age(home: Path) -> None:
    assert "jane-fly" not in find(home, "fly", "-p", "dotfiles").stdout
    assert "jane-fly" in find(home, "fly", "-p", "jane").stdout
    assert "jane-fly" not in find(home, "fly", "--days", "3").stdout


def test_codex_sessions_are_searched_without_injected_instructions(home: Path) -> None:
    result = find(home, "helix")
    assert (
        "  cd /home/ben/.dotfiles && codex resume 019feb7a-26bc-7381-bb87-bb3928675607"
        in result.stdout.splitlines()
    )
    assert find(home, "agents.md").returncode == 1


def test_the_searching_session_is_left_out(home: Path) -> None:
    assert find(home, "anat", session="daysy-cv").returncode == 1
