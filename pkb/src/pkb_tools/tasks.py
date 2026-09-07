"""Task files: one markdown file per recurring job in the notebook's tasks/.

    ---
    title: Daily inbox triage
    schedule: 30 6 * * *        # cron syntax, local time
    command: pkb-triage         # optional: run this instead of an agent
    profile: codex-sub          # optional: agent-run profile (default: its default)
    model: sonnet               # optional, runner-specific
    ---
    (body: the prompt for an agent task; ignored for a command task)

Due-ness is stamp-based: a task is due when its schedule has fired since its
last run. A task never run is due at the next poll. Stamps are machine-local,
which is why only one host runs the timer.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

from cronsim import CronSim, CronSimError

from pkb_tools.notebook import notebook_dir, state_dir


@dataclass(frozen=True)
class Task:
    name: str
    schedule: str
    prompt: str
    command: str = ""
    profile: str = ""
    model: str = ""

    @property
    def kind(self) -> str:
        return "command" if self.command else "agent"


def parse_frontmatter(text: str) -> tuple[dict[str, str], str]:
    """Flat `key: value` frontmatter only; the body is everything after."""
    lines = text.splitlines()
    if not lines or lines[0].strip() != "---":
        return {}, text
    fields: dict[str, str] = {}
    for i, line in enumerate(lines[1:], start=1):
        if line.strip() == "---":
            return fields, "\n".join(lines[i + 1 :]).strip()
        key, sep, value = line.partition(":")
        if sep:
            fields[key.strip()] = value.split("#", 1)[0].strip()
    return fields, ""


def parse_task(path: Path) -> Task | None:
    fields, body = parse_frontmatter(path.read_text())
    schedule = fields.get("schedule", "")
    if not schedule:
        return None
    try:
        CronSim(schedule, datetime.now().astimezone())
    except CronSimError as error:
        raise ValueError(f"{path.name}: bad schedule {schedule!r}: {error}") from error
    return Task(
        name=path.stem,
        schedule=schedule,
        prompt=body,
        command=fields.get("command", ""),
        profile=fields.get("profile", ""),
        model=fields.get("model", ""),
    )


def load_tasks() -> list[Task]:
    tasks_dir = notebook_dir() / "tasks"
    if not tasks_dir.is_dir():
        return []
    tasks = [parse_task(p) for p in sorted(tasks_dir.glob("*.md"))]
    return [t for t in tasks if t is not None]


def stamp_path(task: Task) -> Path:
    return state_dir() / f"{task.name}.last"


def last_run(task: Task) -> datetime | None:
    path = stamp_path(task)
    if not path.exists():
        return None
    return datetime.fromisoformat(path.read_text().strip()).astimezone()


def record_run(task: Task, when: datetime) -> None:
    stamp_path(task).parent.mkdir(parents=True, exist_ok=True)
    stamp_path(task).write_text(when.isoformat())


def next_elapse(task: Task, base: datetime) -> datetime:
    return next(CronSim(task.schedule, base))


def is_due(task: Task, now: datetime) -> bool:
    last = last_run(task)
    return last is None or next_elapse(task, last) <= now
