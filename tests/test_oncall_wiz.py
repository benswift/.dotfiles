#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run unit-oncall's and launchd-run's recovery against the real wiz.

When a failed job succeeds again, both delete the bot tasks its failures filed,
through wiz remove. The anu vault is tests/fixtures/todo/anu, whose two
backup.service tasks stand in for a job that failed twice. Like
test_todo_wiz.py, this stays local: CI can't build the engine.
"""

from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).parent.parent
FIXTURE = ROOT / "tests" / "fixtures" / "todo" / "anu"
WIZ = os.environ.get("WIZ") or shutil.which("wiz")
UNIT = "backup.service"

pytestmark = pytest.mark.skipif(
    WIZ is None or shutil.which("jq") is None, reason="needs wiz and jq on PATH"
)


def run(env: dict[str, str], *args: str) -> str:
    return subprocess.run(
        args, capture_output=True, text=True, env=env, check=True
    ).stdout.rstrip("\n")


@pytest.fixture
def anu(tmp_path: Path) -> tuple[Path, dict[str, str]]:
    """The fixture anu vault with a bare remote, and an environment that
    registers it and keeps every state file in tmp_path."""
    gitconfig = tmp_path / "gitconfig"
    gitconfig.write_text(
        "[user]\n\tname = Test\n\temail = test@example.com\n"
        "[init]\n\tdefaultBranch = main\n[commit]\n\tgpgsign = false\n"
    )
    env = {
        **os.environ,
        "PATH": f"{Path(str(WIZ)).parent}{os.pathsep}{os.environ['PATH']}",
        "GIT_CONFIG_GLOBAL": str(gitconfig),
        "GIT_CONFIG_NOSYSTEM": "1",
        "WIZ_REGISTRY": str(tmp_path / "vaults.toml"),
        "XDG_STATE_HOME": str(tmp_path / "state"),
    }
    vault, remote = tmp_path / "anu", tmp_path / "anu.git"
    shutil.copytree(FIXTURE, vault)
    run(env, "git", "init", "-q", "--bare", str(remote))
    run(env, "git", "init", "-q", str(vault))
    run(env, "git", "-C", str(vault), "add", ".")
    run(env, "git", "-C", str(vault), "commit", "-q", "-m", "fixture")
    run(env, "git", "-C", str(vault), "remote", "add", "origin", str(remote))
    run(env, "git", "-C", str(vault), "push", "-q", "-u", "origin", "main")
    (tmp_path / "vaults.toml").write_text(f'[vaults]\nanu = "{vault}"\n')
    return vault, env


def bot_tasks(vault: Path) -> list[Path]:
    return sorted((vault / "tasks").glob("*-backup-failed.md"))


def link_to_a_bot_task(vault: Path, env: dict[str, str]) -> None:
    (vault / "tasks" / "2026-01-09-look-into-backups.md").write_text(
        "---\ntitle: Look into backups\nstatus: open\n---\n\n"
        f"See [[tasks/{bot_tasks(vault)[0].stem}]].\n"
    )
    run(env, "git", "-C", str(vault), "add", ".")
    run(env, "git", "-C", str(vault), "commit", "-q", "-m", "link")


def unit_oncall_clear(env: dict[str, str]) -> str:
    """Clear the unit as a success would, returning unit-oncall's log."""
    run(env, str(ROOT / "bin" / "unit-oncall"), "clear", UNIT)
    return (Path(env["XDG_STATE_HOME"]) / "unit-oncall" / "unit-oncall.log").read_text()


def test_unit_oncall_deletes_and_shares_a_recovered_units_tasks(
    anu: tuple[Path, dict[str, str]],
) -> None:
    vault, env = anu
    assert len(bot_tasks(vault)) == 2
    log = unit_oncall_clear(env)
    assert bot_tasks(vault) == []
    assert "recovered; deleted tasks/" in log
    git = ("git", "-C", str(vault))
    assert run(env, *git, "status", "--porcelain") == ""
    assert run(env, *git, "log", "-1", "--format=%s") == f"oncall: {UNIT} recovered"
    assert run(env, *git, "rev-parse", "HEAD") == run(
        env, *git, "rev-parse", "origin/main"
    )


def test_unit_oncall_keeps_tasks_something_links_to(
    anu: tuple[Path, dict[str, str]],
) -> None:
    vault, env = anu
    link_to_a_bot_task(vault, env)
    log = unit_oncall_clear(env)
    assert len(bot_tasks(vault)) == 2
    assert "recovered, but FAILED to delete tasks/" in log
    assert run(env, "git", "-C", str(vault), "log", "-1", "--format=%s") == "link"


def launchd_run_recover(tmp_path: Path, env: dict[str, str]) -> str:
    """Run a succeeding job after a recorded failure, as launchd would,
    returning launchd-run's log. It sets its own PATH from HOME, so HOME is a
    folder with wiz in its ~/.local/bin."""
    home = tmp_path / "home"
    (home / ".local" / "bin").mkdir(parents=True)
    (home / ".local" / "bin" / "wiz").symlink_to(str(WIZ))
    state = Path(env["XDG_STATE_HOME"]) / "launchd-run"
    state.mkdir(parents=True)
    (state / f"{UNIT}.failed").write_text("first failure\n")
    run(
        {**env, "HOME": str(home)},
        str(ROOT / "bin" / "launchd-run"),
        UNIT,
        "--",
        "true",
    )
    return (state / "launchd-run.log").read_text()


def test_launchd_run_deletes_a_recovered_jobs_tasks(
    anu: tuple[Path, dict[str, str]], tmp_path: Path
) -> None:
    vault, env = anu
    log = launchd_run_recover(tmp_path, env)
    assert bot_tasks(vault) == []
    assert "deleted anu:tasks/" in log
    # anu defers its commits, which wiz due's next sync makes.
    status = run(env, "git", "-C", str(vault), "status", "--porcelain")
    assert sorted(status.splitlines()) == [
        " D tasks/2026-01-04-backup-failed.md",
        " D tasks/2026-01-05-backup-failed.md",
    ]


def test_launchd_run_keeps_tasks_something_links_to(
    anu: tuple[Path, dict[str, str]], tmp_path: Path
) -> None:
    vault, env = anu
    link_to_a_bot_task(vault, env)
    log = launchd_run_recover(tmp_path, env)
    assert len(bot_tasks(vault)) == 2
    assert "FAILED to delete anu:tasks/" in log


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-q", "-n", "auto"]))
