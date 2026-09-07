from __future__ import annotations

from datetime import datetime
from pathlib import Path

import pytest
from pkb_tools import tasks as T


def write_task(
    notebook: Path, name: str, frontmatter: str, body: str = "Do it."
) -> Path:
    path = notebook / "tasks" / f"{name}.md"
    path.write_text(f"---\n{frontmatter}\n---\n\n{body}\n")
    return path


def test_parse_agent_task(notebook: Path) -> None:
    path = write_task(
        notebook, "sweep", "title: Sweep\nschedule: 30 6 * * 1  # monday\nmodel: sonnet"
    )
    task = T.parse_task(path)
    assert task is not None
    assert task.kind == "agent"
    assert task.schedule == "30 6 * * 1"
    assert task.model == "sonnet"
    assert task.prompt == "Do it."


def test_parse_command_task(notebook: Path) -> None:
    path = write_task(
        notebook, "health", "schedule: 0 7 * * *\ncommand: pkb-health data/health.toml"
    )
    task = T.parse_task(path)
    assert task is not None
    assert task.kind == "command"
    assert task.command == "pkb-health data/health.toml"


def test_file_without_schedule_is_not_a_task(notebook: Path) -> None:
    path = notebook / "tasks" / "README.md"
    path.write_text("# tasks\n")
    assert T.parse_task(path) is None


def test_bad_schedule_is_an_error(notebook: Path) -> None:
    path = write_task(notebook, "bad", "schedule: Mon 06:30")
    with pytest.raises(ValueError, match="bad schedule"):
        T.parse_task(path)


def test_due_when_never_run(notebook: Path) -> None:
    task = T.Task(name="x", schedule="0 7 * * *", prompt="p")
    assert T.is_due(task, datetime.now().astimezone())


def test_due_only_after_schedule_fires(notebook: Path) -> None:
    task = T.Task(name="x", schedule="0 7 * * *", prompt="p")
    T.record_run(task, datetime(2026, 9, 7, 7, 1).astimezone())
    assert not T.is_due(task, datetime(2026, 9, 7, 23, 0).astimezone())
    assert T.is_due(task, datetime(2026, 9, 8, 7, 0).astimezone())


def test_naive_legacy_stamp_is_read_as_local(notebook: Path) -> None:
    task = T.Task(name="x", schedule="0 7 * * *", prompt="p")
    T.stamp_path(task).parent.mkdir(parents=True)
    T.stamp_path(task).write_text("2026-09-07T07:00:00")
    last = T.last_run(task)
    assert last is not None and last.tzinfo is not None
