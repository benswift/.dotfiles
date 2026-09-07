"""Paths and shell helpers shared by every command."""

from __future__ import annotations

import os
import subprocess
from pathlib import Path


def notebook_dir() -> Path:
    return Path(os.environ.get("PKB_NOTEBOOK", "~/.nb/home")).expanduser()


def state_dir() -> Path:
    """Machine-local run state (stamps, logs, caches). Never synced."""
    return Path(
        os.environ.get("PKB_STATE_DIR", "~/.local/state/pkb-agent")
    ).expanduser()


def nb(*args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    """Run nb non-interactively against the notebook.

    `nb` resolves to ~/.dotfiles/bin/nb on PATH, which routes through the
    mise-pinned release even in agent and systemd shells. stdin is /dev/null:
    an inherited pipe makes `nb edit --content` wait on it, which is how the
    old health checks appended the same block twice.
    """
    return subprocess.run(
        ["nb", *args],
        cwd=notebook_dir(),
        capture_output=True,
        text=True,
        stdin=subprocess.DEVNULL,
        check=check,
    )


def git(*args: str) -> str:
    result = subprocess.run(
        ["git", *args],
        cwd=notebook_dir(),
        capture_output=True,
        text=True,
        check=True,
    )
    return result.stdout.strip()


def git_fingerprint() -> str:
    """HEAD plus the working-tree status: changes if anything was written."""
    return git("rev-parse", "HEAD") + "\n" + git("status", "--porcelain")
