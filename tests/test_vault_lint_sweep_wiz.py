#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Run vault-lint-sweep against the real wiz, on a vault built here.

The vault holds one of each thing the sweep looks for, beside one of each it
must leave alone. Like test_oncall_wiz.py, this stays local: CI can't build
the engine.
"""

from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).parent.parent
TYPES = ROOT / "tests" / "fixtures" / "todo" / "anu" / "_types"
WIZ = os.environ.get("WIZ") or shutil.which("wiz")

pytestmark = pytest.mark.skipif(WIZ is None, reason="needs wiz on PATH")

NOTE_TYPE = """---
name: note
folder: notes
match:
  - notes/*.md
filename: dated
schema:
  type: object
  properties:
    title: { type: string, minLength: 1 }
    about: { type: array, items: { $ref: common.json#/$defs/link } }
  additionalProperties: false
---

A note.
"""

PERSON_TYPE = """---
name: person
folder: people
match:
  - people/*.md
filename: kebab
schema:
  type: object
  properties:
    title: { type: string, minLength: 1 }
    email: { type: string }
  additionalProperties: false
---

A person.
"""

COPY = (
    "The budget covers salaries, travel, equipment and the milestones for the grant.\n"
)

OLD = {
    "people/ann-lee.md": "---\ntitle: Ann Lee\n---\n\nColleague.\n",
    "notes/2020-01-01-demo.md": "---\ntitle: Demo plans\n---\n\nThe demo is planned for 2020-02-01.\n",
}
NEW = {
    "notes/2026-01-02-meeting.md": "---\ntitle: Meeting\n---\n\nMet Ann Lee about the plan.\n",
    "notes/2026-01-03-linked.md": "---\ntitle: Linked\n---\n\nMet [[people/ann-lee|Ann Lee]] again.\n",
    "notes/2026-01-04-grant.md": "---\ntitle: Grant budget\n---\n\n" + COPY,
    "notes/2026-01-05-grant-copy.md": "---\ntitle: The grant's budget\n---\n\n" + COPY,
    "tasks/2026-01-06-disk-failed.md": "---\ntitle: disk 1 failed\nstatus: open\ntags: [bot]\n---\n",
    "tasks/2026-01-07-disk-failed.md": "---\ntitle: disk 2 failed\nstatus: open\ntags: [bot]\n---\n",
}


def run(env: dict[str, str], *args: str) -> str:
    proc = subprocess.run(args, capture_output=True, text=True, env=env, check=False)
    assert proc.returncode == 0, proc.stderr
    return proc.stdout


@pytest.fixture
def sweep(tmp_path: Path) -> str:
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
    }
    vault = tmp_path / "anu"
    shutil.copytree(TYPES, vault / "_types")
    (vault / "_types" / "note.md").write_text(NOTE_TYPE)
    (vault / "_types" / "person.md").write_text(PERSON_TYPE)
    (vault / "mdbase.yaml").write_text("format: 1\nname: anu\ndefault_role: anyone\n")
    run(env, "git", "init", "-q", str(vault))
    for files, when in [(OLD, "2020-01-01T00:00:00"), (NEW, None)]:
        for name, text in files.items():
            (vault / name).parent.mkdir(exist_ok=True)
            (vault / name).write_text(text)
        dated = {"GIT_AUTHOR_DATE": when, "GIT_COMMITTER_DATE": when} if when else {}
        run({**env, **dated}, "git", "-C", str(vault), "add", ".")
        run({**env, **dated}, "git", "-C", str(vault), "commit", "-q", "-m", "x")
    (tmp_path / "vaults.toml").write_text(f'[vaults]\nanu = "{vault}"\n')
    out = run(
        env,
        str(ROOT / "bin" / "vault-lint-sweep"),
        "--vault",
        "anu",
        "--slices",
        "1",
    )
    return out


def section(out: str, title: str) -> str:
    return out.split(f"## {title}")[1].split("\n## ")[0]


def test_lists_a_name_in_prose_only_where_unlinked(sweep: str) -> None:
    names = section(sweep, "Unlinked names")
    assert "anu:notes/2026-01-02-meeting.md` names Ann Lee" in names
    assert "2026-01-03-linked" not in names


def test_lists_near_copies(sweep: str) -> None:
    copies = section(sweep, "Near-copies")
    assert (
        "anu:notes/2026-01-04-grant.md` and `anu:notes/2026-01-05-grant-copy.md"
        in copies
    )


def test_lists_bot_tasks_filed_again(sweep: str) -> None:
    assert "2 tasks titled like" in section(sweep, "Repeated bot tasks")


def test_lists_a_claim_whose_date_has_passed(sweep: str) -> None:
    stale = section(sweep, "Stale claims")
    assert "anu:notes/2020-01-01-demo.md`: The demo is planned for 2020-02-01." in stale
    assert "meeting" not in stale


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-q", "-n", "auto"]))
