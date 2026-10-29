#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run ingest-claude-logs against a temporary HOME and check its staleness alarm."""

from __future__ import annotations

import json
import os
import sqlite3
import subprocess
import sys
import time
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "ingest-claude-logs"
OLD = "2020-01-01T00:00:00.000Z"
NOW = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())


def ship_session(
    home: Path, host: str, timestamp: str, name: str = "", **fields: str
) -> None:
    project = home / "claude-logs" / host / "-home-ben-project"
    project.mkdir(parents=True, exist_ok=True)
    record = {"timestamp": timestamp, "cwd": "/home/ben/project", "version": "1"}
    (project / f"{name or host}-session.jsonl").write_text(
        json.dumps(record | fields) + "\n"
    )


def ship_codex_session(home: Path, host: str, originator: str) -> None:
    shard = home / "codex-logs" / host / "2026" / "10" / "30"
    shard.mkdir(parents=True, exist_ok=True)
    meta = {
        "type": "session_meta",
        "payload": {"timestamp": NOW, "cwd": "/home/ben/x", "originator": originator},
    }
    name = f"rollout-2026-10-30T09-00-00-{originator.replace('_', '-')}.jsonl"
    (shard / name).write_text(json.dumps(meta) + "\n")


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


def interactive_by_file(home: Path) -> dict[str, int | None]:
    with sqlite3.connect(home / "claude-logs" / "analytics.db") as conn:
        rows = conn.execute("SELECT file_path, interactive FROM sessions")
        return {Path(path).name: flag for path, flag in rows}


def test_records_whether_a_person_drove_the_session(home: Path) -> None:
    ship_session(home, "weddle", NOW, name="tui", entrypoint="cli")
    ship_session(home, "weddle", NOW, name="print", entrypoint="sdk-cli")
    ship_session(home, "weddle", NOW, name="unknown")
    ship_codex_session(home, "weddle", "codex-tui")
    ship_codex_session(home, "weddle", "codex_exec")
    result = ingest(home)
    assert result.returncode == 0, result.stderr
    assert interactive_by_file(home) == {
        "tui-session.jsonl": 1,
        "print-session.jsonl": 0,
        "unknown-session.jsonl": None,
        "rollout-2026-10-30T09-00-00-codex-tui.jsonl": 1,
        "rollout-2026-10-30T09-00-00-codex-exec.jsonl": 0,
    }


def test_database_missing_a_column_is_rebuilt(home: Path) -> None:
    with sqlite3.connect(home / "claude-logs" / "analytics.db") as conn:
        conn.execute("CREATE TABLE sessions (file_path TEXT PRIMARY KEY, agent TEXT)")
    ship_session(home, "weddle", NOW, entrypoint="cli")
    result = ingest(home)
    assert result.returncode == 0, result.stderr
    assert interactive_by_file(home) == {"weddle-session.jsonl": 1}


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
