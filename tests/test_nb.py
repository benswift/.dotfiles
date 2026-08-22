#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Smoke-test the pinned nb release against an isolated notebook."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import pytest
import tomllib

ROOT = Path(__file__).parent.parent
MISE_CONFIG = ROOT / "mise" / "config.toml"
NB_COMMAND = (str(ROOT / "bin" / "nb"),)


def run_nb(
    env: dict[str, str], *args: str, check: bool = True
) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        [*NB_COMMAND, *args],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    if check and result.returncode != 0:
        pytest.fail(
            f"nb {' '.join(args) or '<init>'} exited {result.returncode}\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}"
        )
    return result


@pytest.fixture
def isolated_nb(tmp_path: Path) -> tuple[dict[str, str], Path]:
    notebook_root = tmp_path / "nb"
    env = os.environ.copy()
    env.update(
        {
            "GIT_AUTHOR_EMAIL": "nb-smoke@example.invalid",
            "GIT_AUTHOR_NAME": "nb smoke test",
            "GIT_COMMITTER_EMAIL": "nb-smoke@example.invalid",
            "GIT_COMMITTER_NAME": "nb smoke test",
            "NB_AUTO_SYNC": "0",
            "NB_DIR": str(notebook_root),
            "NB_EDITOR": "true",
            "NBRC_PATH": str(tmp_path / "nbrc"),
            "NO_COLOR": "1",
            "PAGER": "cat",
            "TERM": "dumb",
        }
    )

    # nb's first ordinary invocation creates the default home notebook and
    # exits after its welcome screen. Initialise explicitly so every test
    # exercises the requested command rather than that first-run path.
    run_nb(env)
    return env, notebook_root / "home"


def test_mise_resolves_configured_version() -> None:
    with MISE_CONFIG.open("rb") as config_file:
        configured = tomllib.load(config_file)["tools"]["github:xwmx/nb"]["version"]
    result = subprocess.run(
        [*NB_COMMAND, "--version"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=True,
    )
    assert result.stdout.strip() == configured


def test_add_show_and_search(isolated_nb: tuple[dict[str, str], Path]) -> None:
    env, notebook = isolated_nb
    run_nb(
        env,
        "add",
        "--filename",
        "smoke.md",
        "--content",
        "distinctive smoke body",
        "--no-color",
    )

    shown = run_nb(env, "show", "smoke.md", "--print", "--no-color")
    assert shown.stdout.strip() == "distinctive smoke body"

    found = run_nb(env, "search", "distinctive smoke", "--list", "--no-color")
    assert "smoke.md" in found.stdout
    assert (notebook / "smoke.md").read_text().strip() == "distinctive smoke body"


def test_daily_note_append_recipe(
    isolated_nb: tuple[dict[str, str], Path],
) -> None:
    env, notebook = isolated_nb
    filename = "20991231.md"
    run_nb(
        env,
        "add",
        "--filename",
        filename,
        "--content",
        "# Daily 2099-12-31\n",
        "--no-color",
    )
    run_nb(
        env,
        "edit",
        filename,
        "--content",
        "A non-interactive journal entry.",
        "--no-color",
    )

    text = (notebook / filename).read_text()
    assert text.startswith("# Daily 2099-12-31")
    assert text.rstrip().endswith("A non-interactive journal entry.")


def test_todo_round_trip(isolated_nb: tuple[dict[str, str], Path]) -> None:
    env, notebook = isolated_nb
    run_nb(env, "todo", "add", "isolated smoke todo", "--no-color")

    todos = run_nb(env, "todos", "open", "--no-color")
    assert "[ ] isolated smoke todo" in todos.stdout
    todo_files = list(notebook.glob("*.todo.md"))
    assert len(todo_files) == 1
    assert todo_files[0].read_text().startswith("# [ ] isolated smoke todo")


def test_sync_failure_is_nonzero(
    isolated_nb: tuple[dict[str, str], Path], tmp_path: Path
) -> None:
    env, notebook = isolated_nb
    run_nb(
        env,
        "add",
        "--filename",
        "unsynced.md",
        "--content",
        "local content",
        "--no-color",
    )
    subprocess.run(
        [
            "git",
            "-C",
            str(notebook),
            "remote",
            "add",
            "origin",
            str(tmp_path / "missing.git"),
        ],
        check=True,
    )

    result = run_nb(env, "sync", "--no-color", check=False)
    assert result.returncode != 0
    assert "Could not read from remote repository" in result.stderr


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
