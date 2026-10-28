#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run ingest-claude-logs against a temporary HOME and check its staleness alarm."""

from __future__ import annotations

import json
import os
import subprocess
import sys
import time
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "ingest-claude-logs"
OLD = "2020-01-01T00:00:00.000Z"


def ship_session(home: Path, host: str, timestamp: str) -> None:
    project = home / "claude-logs" / host / "-home-ben-project"
    project.mkdir(parents=True, exist_ok=True)
    record = {"timestamp": timestamp, "cwd": "/home/ben/project", "version": "1"}
    (project / f"{host}-session.jsonl").write_text(json.dumps(record) + "\n")


def beat(home: Path, host: str, at: float) -> None:
    (home / "claude-logs" / host / ".last-ship").write_text(f"{int(at)}\n")


def ingest(home: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT)],
        env={**os.environ, "HOME": str(home)},
        capture_output=True,
        text=True,
        check=False,
    )


@pytest.fixture
def home(tmp_path: Path) -> Path:
    (tmp_path / "claude-logs").mkdir()
    return tmp_path


def test_idle_host_with_fresh_heartbeat_passes(home: Path) -> None:
    ship_session(home, "watson", OLD)
    beat(home, "watson", time.time())
    result = ingest(home)
    assert result.returncode == 0, result.stderr
    assert "Ingested 1 new/changed" in result.stdout


def test_stale_heartbeat_fails_even_with_recent_sessions(home: Path) -> None:
    now = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    ship_session(home, "daysy", now)
    beat(home, "daysy", time.time() - 30 * 86400)
    result = ingest(home)
    assert result.returncode != 0
    assert "daysy (last ship" in result.stderr


def test_host_without_heartbeat_falls_back_to_newest_session(home: Path) -> None:
    ship_session(home, "oldhost", OLD)
    result = ingest(home)
    assert result.returncode != 0
    assert f"oldhost (newest session {OLD})" in result.stderr


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
