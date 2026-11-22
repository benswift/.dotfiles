#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run bin/oncall, and unit-oncall's and launchd-run's use of it, against the
real wiz.

A failure files one task per key and adds to it while it is open; a success
deletes the open tasks through wiz remove. The anu vault is
tests/fixtures/todo/anu, whose two backup.service tasks stand in for a job that
failed twice before oncall kept one task per key. Pages go to a stub mise in
the test's HOME. Like test_todo_wiz.py, this stays local: CI can't build the
engine.
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


@pytest.fixture
def home(tmp_path: Path) -> Path:
    """A HOME whose ~/.dotfiles is this repo and whose ~/.local/bin holds wiz
    and a mise that records each page instead of sending it: oncall, and
    launchd-run, put both ahead of the PATH they are given."""
    home = tmp_path / "home"
    bin_dir = home / ".local" / "bin"
    bin_dir.mkdir(parents=True)
    (bin_dir / "wiz").symlink_to(str(WIZ))
    (home / ".dotfiles").symlink_to(ROOT)
    mise = bin_dir / "mise"
    mise.write_text(f'#!/bin/sh\ncat >/dev/null\necho "$@" >>"{tmp_path}/pages"\n')
    mise.chmod(0o755)
    return home


def pages(tmp_path: Path) -> list[str]:
    path = tmp_path / "pages"
    return path.read_text().splitlines() if path.exists() else []


def oncall_log(env: dict[str, str]) -> str:
    return (Path(env["XDG_STATE_HOME"]) / "oncall" / "oncall.log").read_text()


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
    """Clear the unit as a success would, returning oncall's log."""
    run(env, str(ROOT / "bin" / "unit-oncall"), "clear", UNIT)
    return oncall_log(env)


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


def launchd_run_recover(home: Path, env: dict[str, str]) -> str:
    """Run a succeeding job after a recorded failure, as launchd would,
    returning oncall's log."""
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
    return oncall_log(env)


def test_launchd_run_deletes_a_recovered_jobs_tasks(
    anu: tuple[Path, dict[str, str]], home: Path
) -> None:
    vault, env = anu
    log = launchd_run_recover(home, env)
    assert bot_tasks(vault) == []
    assert "recovered; deleted tasks/" in log
    # anu defers its commits, which wiz due's next sync makes.
    status = run(env, "git", "-C", str(vault), "status", "--porcelain")
    assert sorted(status.splitlines()) == [
        " D tasks/2026-01-04-backup-failed.md",
        " D tasks/2026-01-05-backup-failed.md",
    ]


def test_launchd_run_keeps_tasks_something_links_to(
    anu: tuple[Path, dict[str, str]], home: Path
) -> None:
    vault, env = anu
    link_to_a_bot_task(vault, env)
    log = launchd_run_recover(home, env)
    assert len(bot_tasks(vault)) == 2
    assert "FAILED to delete tasks/" in log


def oncall_file(env: dict[str, str], home: Path, body: str, *args: str) -> str:
    return subprocess.run(
        [str(ROOT / "bin" / "oncall"), "file", *args],
        input=body,
        capture_output=True,
        text=True,
        env={**env, "HOME": str(home)},
        check=True,
    ).stdout.strip()


def test_oncall_adds_to_the_open_task_and_pages_once(
    anu: tuple[Path, dict[str, str]], home: Path, tmp_path: Path
) -> None:
    vault, env = anu
    title = "sweep.service failed on weddle"
    first = oncall_file(
        env, home, "First failure.", "-t", "oncall", "sweep.service", title
    )
    again = oncall_file(
        env, home, "Second failure.", "-t", "oncall", "sweep.service", title
    )
    assert first == again
    text = (vault / first).read_text()
    assert "tags: [bot, oncall]" in text or "- oncall" in text
    assert text.rstrip().endswith("First failure.\n\nSecond failure.")
    assert len(pages(tmp_path)) == 1
    git = ("git", "-C", str(vault))
    assert run(env, *git, "status", "--porcelain") == ""
    assert run(env, *git, "rev-parse", "HEAD") == run(
        env, *git, "rev-parse", "origin/main"
    )


def test_oncall_pages_an_append_once_the_last_page_is_a_day_old(
    anu: tuple[Path, dict[str, str]], home: Path, tmp_path: Path
) -> None:
    _, env = anu
    oncall_file(env, home, "First.", "-n", "sweep.service", "sweep failed")
    (Path(env["XDG_STATE_HOME"]) / "oncall" / "sweep.service.paged").write_text("0\n")
    oncall_file(env, home, "A day on.", "-n", "sweep.service", "sweep failed")
    oncall_file(env, home, "Soon after.", "-n", "sweep.service", "sweep failed")
    assert len(pages(tmp_path)) == 2


def test_oncall_quiet_files_without_paging(
    anu: tuple[Path, dict[str, str]], home: Path, tmp_path: Path
) -> None:
    vault, env = anu
    task = oncall_file(
        env, home, "A finding.", "-q", "-n", "comp4020-fly-sweep", "fly-sweep findings"
    )
    assert (vault / task).exists()
    assert pages(tmp_path) == []


def test_unit_oncall_rearm_adds_to_the_open_task(
    anu: tuple[Path, dict[str, str]], home: Path, tmp_path: Path
) -> None:
    """A unit still failing a day after its first failure re-pages onto its
    open task rather than filing a second."""
    vault, env = anu
    env = {**env, "HOME": str(home)}
    unit = "daily.service"
    run(env, str(ROOT / "bin" / "unit-oncall"), "fail", unit)
    marker = Path(env["XDG_STATE_HOME"]) / "unit-oncall" / f"{unit}.failed"
    marker.write_text("first failure 2026-01-01T00:00:00+11:00\n")
    (Path(env["XDG_STATE_HOME"]) / "oncall" / f"{unit}.paged").write_text("0\n")
    run(env, str(ROOT / "bin" / "unit-oncall"), "fail", unit)
    tasks = sorted((vault / "tasks").glob("*-daily-service-failed*.md"))
    assert len(tasks) == 1
    assert tasks[0].read_text().count("Filed automatically by unit-oncall") == 2
    assert len(pages(tmp_path)) == 2


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-q", "-n", "auto"]))
