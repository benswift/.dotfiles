#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Tests for pkb-agent's scheduling and sync safety boundary."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "pkb-agent"
_loader = importlib.machinery.SourceFileLoader("pkb_agent", str(SCRIPT))
_spec = importlib.util.spec_from_loader("pkb_agent", _loader, origin=str(SCRIPT))
assert _spec is not None and _spec.loader is not None
mod = importlib.util.module_from_spec(_spec)
sys.modules["pkb_agent"] = mod
_spec.loader.exec_module(mod)


def due_task() -> mod.Task:
    return mod.Task(
        name="smoke-task",
        schedule="daily",
        model="sonnet",
        prompt="Smoke test.",
    )


def make_one_task_due(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(mod, "load_tasks", lambda: [due_task()])
    monkeypatch.setattr(mod, "is_due", lambda _task, _now: True)


def test_pre_task_sync_failure_aborts_tasks(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    make_one_task_due(monkeypatch)
    phases: list[str] = []

    def fail_sync(phase: str) -> bool:
        phases.append(phase)
        return False

    monkeypatch.setattr(mod, "sync_notebook", fail_sync)

    def unexpected_run(_task: mod.Task) -> int:
        pytest.fail("task ran after the pre-task sync failed")

    monkeypatch.setattr(mod, "run_task", unexpected_run)
    assert mod.cmd_run_due() == 1
    assert phases == ["pre-task"]


def test_post_task_sync_failure_fails_run(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    make_one_task_due(monkeypatch)
    outcomes = iter((True, False))
    phases: list[str] = []

    def sync(phase: str) -> bool:
        phases.append(phase)
        return next(outcomes)

    monkeypatch.setattr(mod, "sync_notebook", sync)
    monkeypatch.setattr(mod, "run_task", lambda _task: 0)
    assert mod.cmd_run_due() == 1
    assert phases == ["pre-task", "post-task"]


def test_post_task_sync_runs_after_unexpected_task_error(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    make_one_task_due(monkeypatch)
    phases: list[str] = []
    monkeypatch.setattr(
        mod,
        "sync_notebook",
        lambda phase: phases.append(phase) is None,
    )

    def fail_task(_task: mod.Task) -> int:
        raise RuntimeError("task failed unexpectedly")

    monkeypatch.setattr(mod, "run_task", fail_task)
    with pytest.raises(RuntimeError, match="unexpectedly"):
        mod.cmd_run_due()
    assert phases == ["pre-task", "post-task"]


def test_sync_notebook_reports_nb_failure(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    monkeypatch.setattr(mod, "NOTEBOOK", tmp_path)
    monkeypatch.setattr(
        mod.subprocess,
        "run",
        lambda *_args, **_kwargs: subprocess.CompletedProcess(
            args=["nb", "sync"],
            returncode=23,
            stdout="sync stdout",
            stderr="sync stderr",
        ),
    )

    assert mod.sync_notebook("pre-task") is False
    captured = capsys.readouterr()
    assert "pre-task sync failed (exit 23)" in captured.err
    assert "sync stdout" in captured.err
    assert "sync stderr" in captured.err


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
