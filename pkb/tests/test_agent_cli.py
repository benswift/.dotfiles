"""Scheduling, sync-safety and failure-detection boundary of pkb-agent."""

from __future__ import annotations

from pathlib import Path

import pytest
from pkb_tools import agent_cli as cli
from pkb_tools import tasks as T
from pkb_tools.agent import AgentFailure


def agent_task() -> T.Task:
    return T.Task(name="smoke", schedule="0 7 * * *", prompt="Smoke test.")


def make_one_task_due(
    monkeypatch: pytest.MonkeyPatch, task: T.Task | None = None
) -> None:
    monkeypatch.setattr(T, "load_tasks", lambda: [task or agent_task()])
    monkeypatch.setattr(T, "is_due", lambda _task, _now: True)


def test_pre_task_sync_failure_aborts_tasks(
    monkeypatch: pytest.MonkeyPatch, notebook: Path
) -> None:
    make_one_task_due(monkeypatch)
    phases: list[str] = []

    def fail_sync(phase: str) -> bool:
        phases.append(phase)
        return False

    monkeypatch.setattr(cli, "sync_notebook", fail_sync)
    monkeypatch.setattr(
        cli, "run_task", lambda _t: pytest.fail("task ran after a failed pre-sync")
    )
    assert cli.cmd_run_due() == 1
    assert phases == ["pre-task"]


def test_post_task_sync_failure_fails_run(
    monkeypatch: pytest.MonkeyPatch, notebook: Path
) -> None:
    make_one_task_due(monkeypatch)
    outcomes = iter((True, False))
    monkeypatch.setattr(cli, "sync_notebook", lambda _phase: next(outcomes))
    monkeypatch.setattr(cli, "run_task", lambda _t: 0)
    assert cli.cmd_run_due() == 1


def test_post_task_sync_runs_after_unexpected_error(
    monkeypatch: pytest.MonkeyPatch, notebook: Path
) -> None:
    make_one_task_due(monkeypatch)
    phases: list[str] = []
    monkeypatch.setattr(
        cli, "sync_notebook", lambda phase: phases.append(phase) is None
    )

    def explode(_t: T.Task) -> int:
        raise RuntimeError("unexpectedly")

    monkeypatch.setattr(cli, "run_task", explode)
    with pytest.raises(RuntimeError, match="unexpectedly"):
        cli.cmd_run_due()
    assert phases == ["pre-task", "post-task"]


def test_agent_task_that_changes_nothing_fails(
    monkeypatch: pytest.MonkeyPatch, notebook: Path
) -> None:
    monkeypatch.setattr(
        cli,
        "run_agent",
        lambda *_a, **k: k["log"].write_text("I'll continue once the scan completes."),
    )
    assert cli.run_task(agent_task()) == 1
    assert T.last_run(agent_task()) is not None


def test_agent_task_that_writes_a_note_succeeds(
    monkeypatch: pytest.MonkeyPatch, notebook: Path
) -> None:
    def fake_agent(*_a: object, **k: object) -> None:
        (notebook / "new-note.md").write_text("hello\n")
        Path(str(k["log"])).write_text("wrote a note")

    monkeypatch.setattr(cli, "run_agent", fake_agent)
    assert cli.run_task(agent_task()) == 0


def test_agent_failure_is_reported_not_raised(
    monkeypatch: pytest.MonkeyPatch, notebook: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    def refuse(*_a: object, **_k: object) -> None:
        raise AgentFailure("provider refused the run")

    monkeypatch.setattr(cli, "run_agent", refuse)
    assert cli.run_task(agent_task()) == 1
    assert "provider refused" in capsys.readouterr().err


def test_command_task_runs_from_notebook(notebook: Path) -> None:
    task = T.Task(
        name="touch",
        schedule="0 7 * * *",
        prompt="",
        command="sh -c 'echo $PKB_TASK > ran.txt'",
    )
    assert cli.run_task(task) == 0
    assert (notebook / "ran.txt").read_text().strip() == "touch"


def test_command_task_failure_is_reported(notebook: Path) -> None:
    task = T.Task(
        name="fail",
        schedule="0 7 * * *",
        prompt="",
        command="sh -c 'echo nope; exit 4'",
    )
    assert cli.run_task(task) == 1
