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
        out = self.run("wiz", "query", "ben", "lx", "-json", 'type == "task"')
        return {r["title"]: r["fields"].get("status", "") for r in json.loads(out)}


@pytest.fixture
def vaults(tmp_path: Path) -> Vaults:
    """Fresh ben and lx vaults, registered in a registry of their own."""
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
    for name in ("ben", "lx"):
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
      1  Book dentist
      2  Renew passport  due 2026-12-01

blocked
      3  Waiting on visa

bot
      4  backup failed  ×2

lx
      5  Send quote

"""


def test_lists_open_tasks_by_section(vaults: Vaults) -> None:
    assert vaults.todo() == LISTING


def test_do_closes_a_whole_bot_row_and_shares_lx_at_once(vaults: Vaults) -> None:
    vaults.todo()
    vaults.todo("do", "4", "5")
    status = vaults.status()
    assert status["backup failed"] == "done"
    assert status["Send quote"] == "done"
    bot = list((vaults.root / "ben" / "tasks").glob("*-backup-failed.md"))
    assert len(bot) == 2
    assert all("status: done" in path.read_text() for path in bot)
    assert vaults.git("lx", "status", "--porcelain") == ""
    assert vaults.git("lx", "rev-parse", "HEAD") == vaults.git(
        "lx", "rev-parse", "origin/main"
    )


def test_block_and_unblock_move_a_task_between_sections(vaults: Vaults) -> None:
    vaults.todo()
    vaults.todo("block", "1")
    assert vaults.status()["Book dentist"] == "blocked"
    assert "      3  Book dentist\n\nbot" in vaults.todo()
    vaults.todo("unblock", "3")
    assert vaults.status()["Book dentist"] == "open"


def test_words_file_a_valid_ben_task(vaults: Vaults) -> None:
    vaults.todo("pay", "henry")
    assert vaults.status()["pay henry"] == "open"
    vaults.run("wiz", "validate", "ben")


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-q", "-n", "auto"]))
