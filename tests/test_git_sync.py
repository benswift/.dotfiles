#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run git-sync in a clone of a temporary bare remote, against a second clone
standing in for another machine."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "git-sync"
ENV = {
    **os.environ,
    "GIT_AUTHOR_NAME": "t",
    "GIT_AUTHOR_EMAIL": "t@example.com",
    "GIT_COMMITTER_NAME": "t",
    "GIT_COMMITTER_EMAIL": "t@example.com",
    "GIT_CONFIG_GLOBAL": os.devnull,
}


def git(repo: Path, *args: str) -> str:
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        env=ENV,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()


def commit(repo: Path, name: str, content: str = "") -> None:
    (repo / name).write_text(content or name)
    git(repo, "add", name)
    git(repo, "commit", "-qm", name)


def sync(repo: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(SCRIPT)], cwd=repo, env=ENV, capture_output=True, text=True, check=False
    )


@pytest.fixture
def repos(tmp_path: Path) -> tuple[Path, Path]:
    """`here` (where git sync runs) and `there`, both cloned from one remote."""
    remote = tmp_path / "remote.git"
    subprocess.run(
        ["git", "init", "-q", "--bare", "-b", "main", str(remote)], env=ENV, check=True
    )
    here, there = tmp_path / "here", tmp_path / "there"
    for clone in (here, there):
        subprocess.run(
            ["git", "clone", "-q", str(remote), str(clone)], env=ENV, check=True
        )
    commit(here, "base")
    git(here, "push", "-q", "-u", "origin", "main")
    git(there, "pull", "-q", "origin", "main")
    git(there, "branch", "-q", "-u", "origin/main")
    return here, there


def test_up_to_date(repos: tuple[Path, Path]) -> None:
    here, _ = repos
    result = sync(here)
    assert result.returncode == 0, result.stderr
    assert "up to date" in result.stdout


def test_behind_fast_forwards(repos: tuple[Path, Path]) -> None:
    here, there = repos
    commit(there, "theirs")
    git(there, "push", "-q")
    result = sync(here)
    assert result.returncode == 0, result.stderr
    assert (here / "theirs").exists()


def test_ahead_pushes(repos: tuple[Path, Path]) -> None:
    here, there = repos
    commit(here, "mine")
    result = sync(here)
    assert result.returncode == 0, result.stderr
    git(there, "pull", "-q")
    assert (there / "mine").exists()


def test_diverged_rebases_then_pushes(repos: tuple[Path, Path]) -> None:
    here, there = repos
    commit(there, "theirs")
    git(there, "push", "-q")
    commit(here, "mine")
    result = sync(here)
    assert result.returncode == 0, result.stderr
    assert git(here, "rev-parse", "HEAD") == git(here, "rev-parse", "origin/main")
    assert git(here, "rev-list", "--merges", "HEAD") == ""


def test_uncommitted_changes_survive_the_rebase(repos: tuple[Path, Path]) -> None:
    here, there = repos
    commit(there, "theirs")
    git(there, "push", "-q")
    (here / "base").write_text("edited, not committed")
    result = sync(here)
    assert result.returncode == 0, result.stderr
    assert (here / "theirs").exists()
    assert (here / "base").read_text() == "edited, not committed"


def test_conflict_is_aborted_and_leaves_the_branch_alone(
    repos: tuple[Path, Path],
) -> None:
    here, there = repos
    commit(there, "clash", "theirs")
    git(there, "push", "-q")
    commit(here, "clash", "mine")
    before = git(here, "rev-parse", "HEAD")
    result = sync(here)
    assert result.returncode == 1
    assert "conflicted" in result.stderr
    assert git(here, "rev-parse", "HEAD") == before
    assert git(here, "status", "--porcelain") == ""


def test_no_upstream_is_refused(repos: tuple[Path, Path]) -> None:
    here, _ = repos
    git(here, "switch", "-qc", "topic")
    result = sync(here)
    assert result.returncode == 1
    assert "no upstream" in result.stderr


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
