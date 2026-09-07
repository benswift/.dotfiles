"""pkb-agent: run the notebook's recurring tasks on their schedules.

  pkb-agent list        show tasks, schedules, last run and next due time
  pkb-agent run <task>  run one task now, regardless of schedule
  pkb-agent run-due     run every task whose schedule has fired since its
                        last run (what the systemd timer calls)

A command task runs its `command:` from the notebook directory. An agent task
hands its body to agent-run as the prompt, from the same directory so the
notebook's CLAUDE.md is the project instructions, and counts as failed unless
the notebook changed --- see pkb_tools.agent for why. Any failure makes
`run-due` exit non-zero, which the service's OnFailure= turns into a todo.
Per-run output lands in the state dir under logs/<task>/.
"""

from __future__ import annotations

import os
import shlex
import subprocess
import sys
from datetime import datetime
from pathlib import Path

from pkb_tools import tasks as T
from pkb_tools.agent import AgentFailure, run_agent, tail
from pkb_tools.notebook import git_fingerprint, nb, notebook_dir, state_dir

TASK_TIMEOUT_S = 3600
NB_SYNC_TIMEOUT_S = 900


def log_path(task: T.Task, now: datetime) -> Path:
    return state_dir() / "logs" / task.name / f"{now:%Y%m%d-%H%M%S}.log"


def run_command_task(task: T.Task, log: Path) -> None:
    env = dict(os.environ, PKB_TASK=task.name, PKB_STATE_DIR=str(state_dir()))
    with log.open("w") as handle:
        result = subprocess.run(
            shlex.split(task.command),
            cwd=notebook_dir(),
            stdout=handle,
            stderr=subprocess.STDOUT,
            stdin=subprocess.DEVNULL,
            env=env,
            timeout=TASK_TIMEOUT_S,
            check=False,
        )
    if result.returncode != 0:
        raise AgentFailure(
            f"{task.command!r} exited {result.returncode}: {tail(log.read_text())}"
        )


def run_agent_task(task: T.Task, log: Path) -> None:
    before = git_fingerprint()
    run_agent(
        task.prompt,
        cwd=notebook_dir(),
        profile=task.profile,
        model=task.model,
        timeout=TASK_TIMEOUT_S,
        log=log,
    )
    if git_fingerprint() == before:
        raise AgentFailure(
            "run finished but changed nothing in the notebook: " + tail(log.read_text())
        )


def run_task(task: T.Task) -> int:
    now = datetime.now().astimezone()
    log = log_path(task, now)
    log.parent.mkdir(parents=True, exist_ok=True)
    print(f"pkb-agent: running {task.name} ({task.kind}) -> {log}")
    try:
        if task.command:
            run_command_task(task, log)
        else:
            run_agent_task(task, log)
    except AgentFailure as error:
        print(f"pkb-agent: {task.name} FAILED: {error}", file=sys.stderr)
        rc = 1
    else:
        print(f"pkb-agent: {task.name} finished (ok)")
        rc = 0
    # Stamped either way: a task that fails on every poll would otherwise
    # re-run every 15 minutes. The failure reaches Ben through OnFailure=.
    T.record_run(task, now)
    return rc


def sync_notebook(phase: str) -> bool:
    try:
        result = nb("sync", check=False)
    except FileNotFoundError as error:
        print(f"pkb-agent: {phase} sync could not start: {error}", file=sys.stderr)
        return False
    except subprocess.TimeoutExpired:
        print(f"pkb-agent: {phase} sync timed out", file=sys.stderr)
        return False
    if result.returncode == 0:
        return True
    print(f"pkb-agent: {phase} sync failed (exit {result.returncode})", file=sys.stderr)
    for part in (result.stdout, result.stderr):
        if part.strip():
            print(part.strip(), file=sys.stderr)
    return False


def cmd_list() -> int:
    now = datetime.now().astimezone()
    tasks = T.load_tasks()
    if not tasks:
        print(f"No tasks in {notebook_dir() / 'tasks'}")
        return 0
    for task in tasks:
        last = T.last_run(task)
        if last is None:
            status = "never run (due now)"
        elif T.is_due(task, now):
            status = f"last {last:%Y-%m-%d %H:%M}, due now"
        else:
            status = f"last {last:%Y-%m-%d %H:%M}, next {T.next_elapse(task, last):%a %Y-%m-%d %H:%M}"
        print(f"{task.name:22} {task.kind:8} {task.schedule:16} {status}")
    return 0


def cmd_run(name: str) -> int:
    by_name = {t.name: t for t in T.load_tasks()}
    if name not in by_name:
        print(f"pkb-agent: no task {name!r}", file=sys.stderr)
        return 1
    return run_task(by_name[name])


def cmd_run_due() -> int:
    now = datetime.now().astimezone()
    due = [t for t in T.load_tasks() if T.is_due(t, now)]
    if not due:
        return 0
    # Pull before tasks write, push after. A failed pull is a hard stop:
    # running against stale state is how two hosts make avoidable conflicts.
    if not sync_notebook("pre-task"):
        print(
            "pkb-agent: refusing to run due tasks against an unsynced notebook",
            file=sys.stderr,
        )
        return 1
    rc = 0
    try:
        for task in due:
            rc |= run_task(task)
    finally:
        if not sync_notebook("post-task"):
            rc |= 1
    return rc


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    try:
        match args:
            case ["list"]:
                return cmd_list()
            case ["run", name]:
                return cmd_run(name)
            case ["run-due"]:
                return cmd_run_due()
            case _:
                print(__doc__, file=sys.stderr)
                return 2
    except ValueError as error:
        print(f"pkb-agent: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
