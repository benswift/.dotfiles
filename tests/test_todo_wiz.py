#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run bin/todo against the real wiz, to catch drift in the engine's CLI.

todo leans on wiz's flag spellings, its -json record shape and exit code 1
meaning "fine". test_todo.py stubs wiz out, so only this suite notices when a
new engine changes any of them. bin/wiz-build runs it after every build.

The vaults are tests/fixtures/todo, copied into git repos with bare remotes so
lx's `wiz sync` has somewhere to push. CI can't build the engine (its repo is
private), so this suite stays local, like test_helix.py.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
from dataclasses import dataclass, field
from pathlib import Path

import pytest

ROOT = Path(__file__).parent.parent
SCRIPT = ROOT / "bin" / "todo"
FIXTURES = ROOT / "tests" / "fixtures" / "todo"
# wiz-build points this at the binary it just built.
WIZ = os.environ.get("WIZ") or shutil.which("wiz")

pytestmark = pytest.mark.skipif(WIZ is None, reason="no wiz on PATH")


@dataclass(frozen=True)
class Vaults:
    """The fixture vaults and the environment that reaches them. The
    environment stays out of the repr, because pytest prints a failing test's
    arguments and this one carries every secret in the caller's."""

    root: Path
    env: dict[str, str] = field(repr=False)

    def todo(self, *args: str) -> str:
        return self.run(str(SCRIPT), *args)

    def run(self, *args: str) -> str:
        return subprocess.run(
            args, capture_output=True, text=True, env=self.env, check=True
        ).stdout

    def git(self, vault: str, *args: str) -> str:
        return self.run("git", "-C", str(self.root / vault), *args).strip()

    def status(self) -> dict[str, str]:
        """Every task's status, by title (the duplicated bot task as one)."""
        out = self.run("wiz", "query", "anu", "ben", "lx", "-json", 'type == "task"')
        return {r["title"]: r["fields"].get("status", "") for r in json.loads(out)}


@pytest.fixture
def vaults(tmp_path: Path) -> Vaults:
    """Fresh anu, ben and lx vaults, registered in a registry of their own."""
    gitconfig = tmp_path / "gitconfig"
    gitconfig.write_text(
        "[user]\n\tname = Test\n\temail = test@example.com\n"
        "[init]\n\tdefaultBranch = main\n"
        "[commit]\n\tgpgsign = false\n"
    )
    v = Vaults(
        tmp_path,
        {
            **os.environ,
            "PATH": f"{Path(str(WIZ)).parent}{os.pathsep}{os.environ['PATH']}",
            "GIT_CONFIG_GLOBAL": str(gitconfig),
            "GIT_CONFIG_NOSYSTEM": "1",
            "WIZ_REGISTRY": str(tmp_path / "vaults.toml"),
            "XDG_STATE_HOME": str(tmp_path / "state"),
        },
    )
    registry = ["[vaults]"]
    for name in ("anu", "ben", "lx"):
        vault, remote = tmp_path / name, tmp_path / f"{name}.git"
        shutil.copytree(FIXTURES / name, vault)
        v.run("git", "init", "-q", "--bare", str(remote))
        v.run("git", "init", "-q", str(vault))
        v.git(name, "add", ".")
        v.git(name, "commit", "-q", "-m", "fixture")
        v.git(name, "remote", "add", "origin", str(remote))
        v.git(name, "push", "-q", "-u", "origin", "main")
        registry.append(f'{name} = "{vault}"')
    (tmp_path / "vaults.toml").write_text("\n".join(registry) + "\n")
    return v


LISTING = """
todos
      1  Mark assignments
      2  Book dentist
      3  Renew passport  due 2026-12-01

blocked
      4  Waiting on reviewer
      5  Waiting on visa

bot
      6  backup failed  ×2

lx
      7  Send quote

"""


def test_lists_open_tasks_by_section(vaults: Vaults) -> None:
    assert vaults.todo() == LISTING


def test_do_closes_a_whole_bot_row_and_shares_lx_at_once(vaults: Vaults) -> None:
    vaults.todo()
    vaults.todo("do", "6", "7")
    status = vaults.status()
    assert status["backup failed"] == "done"
    assert status["Send quote"] == "done"
    bot = list((vaults.root / "anu" / "tasks").glob("*-backup-failed.md"))
    assert len(bot) == 2
    assert all("status: done" in path.read_text() for path in bot)
    assert vaults.git("anu", "status", "--porcelain") == ""
    assert vaults.git("anu", "log", "-1", "--format=%s") == "todo: close 2 tasks"
    assert vaults.git("lx", "status", "--porcelain") == ""
    assert vaults.git("lx", "rev-parse", "HEAD") == vaults.git(
        "lx", "rev-parse", "origin/main"
    )


def test_do_and_undo_commit_a_ben_task_without_sharing(vaults: Vaults) -> None:
    vaults.todo()
    assert vaults.todo("do", "2") == "done: ben:tasks/2026-01-02-book-dentist.md\n"
    assert vaults.status()["Book dentist"] == "done"
    assert vaults.git("ben", "status", "--porcelain") == ""
    assert vaults.git("ben", "log", "-1", "--format=%s").startswith("todo: close ")
    assert vaults.git("ben", "rev-list", "--count", "origin/main..HEAD") == "1"
    vaults.todo("undo", "2")
    assert vaults.status()["Book dentist"] == "open"
    assert vaults.git("ben", "status", "--porcelain") == ""
    assert vaults.git("ben", "log", "-1", "--format=%s").startswith("todo: reopen ")
    assert vaults.git("ben", "rev-list", "--count", "origin/main..HEAD") == "2"


def test_do_commits_only_the_task_it_closed(vaults: Vaults) -> None:
    (vaults.root / "anu" / "scratch.md").write_text("half-written\n")
    vaults.todo()
    vaults.todo("do", "1")
    assert vaults.git("anu", "log", "-1", "--format=%s").startswith("todo: close ")
    assert vaults.git("anu", "status", "--porcelain") == "?? scratch.md"


def test_block_and_unblock_move_a_task_between_sections(vaults: Vaults) -> None:
    vaults.todo()
    vaults.todo("block", "2")
    assert vaults.status()["Book dentist"] == "blocked"
    assert "      5  Book dentist\n\nbot" in vaults.todo()
    vaults.todo("unblock", "5")
    assert vaults.status()["Book dentist"] == "open"


def test_e_opens_every_file_of_the_rows_in_the_editor(vaults: Vaults) -> None:
    log = vaults.root / "editor-args"
    editor = vaults.root / "editor"
    editor.write_text(f'#!/bin/sh\nprintf "%s\\n" "$@" > {log}\n')
    editor.chmod(0o755)
    vaults.todo()
    subprocess.run(
        [str(SCRIPT), "e", "6", "2"],
        env={**vaults.env, "VISUAL": "", "EDITOR": f"{editor} --vsplit"},
        check=True,
    )
    args = log.read_text().splitlines()
    assert args[0] == "--vsplit"
    opened = [Path(a) for a in args[1:]]
    assert all(p.is_file() and p.is_relative_to(vaults.root) for p in opened)
    assert sorted(p.name.endswith("-backup-failed.md") for p in opened) == [
        False,
        True,
        True,
    ]
    assert "Book dentist" in opened[-1].read_text()


def test_words_file_a_valid_anu_task(vaults: Vaults) -> None:
    vaults.todo("pay", "henry")
    assert vaults.status()["pay henry"] == "open"
    assert list((vaults.root / "anu" / "tasks").glob("*-pay-henry.md"))
    vaults.run("wiz", "validate", "anu")
    assert vaults.git("anu", "status", "--porcelain") == ""
    assert vaults.git("anu", "log", "-1", "--format=%s") == "todo: add pay henry"
    assert vaults.git("anu", "rev-list", "--count", "origin/main..HEAD") == "1"


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-q", "-n", "auto"]))
