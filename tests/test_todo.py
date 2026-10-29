#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
from __future__ import annotations

import os
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "todo"


@pytest.fixture
def calls(tmp_path: Path) -> Path:
    """A stand-in wiz on PATH that records each invocation's arguments."""
    log = tmp_path / "wiz-calls"
    fake = tmp_path / "bin" / "wiz"
    fake.parent.mkdir()
    fake.write_text(f'#!/bin/sh\necho "$*" >> {log}\necho "[]"\n')
    fake.chmod(0o755)
    return log


def todo(calls: Path, *args: str) -> subprocess.CompletedProcess[str]:
    env = {
        **os.environ,
        "PATH": f"{calls.parent / 'bin'}{os.pathsep}{os.environ['PATH']}",
        "XDG_STATE_HOME": str(calls.parent / "state"),
    }
    return subprocess.run(
        [str(SCRIPT), *args], capture_output=True, text=True, env=env, check=False
    )


@pytest.mark.parametrize("flag", ["-h", "--help"])
def test_help_prints_usage_and_files_nothing(calls: Path, flag: str) -> None:
    result = todo(calls, flag)
    assert result.returncode == 0
    assert "todo do <n>" in result.stdout
    assert not calls.exists()


def test_unknown_option_is_refused_not_filed(calls: Path) -> None:
    result = todo(calls, "--verbose", "pay", "henry")
    assert result.returncode == 2
    assert "unknown option --verbose" in result.stderr
    assert not calls.exists()


def test_words_still_file_a_task(calls: Path) -> None:
    result = todo(calls, "pay", "henry")
    assert result.returncode == 0
    assert calls.read_text().startswith("add ben task -title pay henry")
