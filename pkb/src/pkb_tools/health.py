"""pkb-health: are the other scheduled jobs alive, and is strproxy up?

  pkb-health <config.toml>

Deterministic, no model involved. A job is healthy when its timer is active,
its last run did not fail, and it completed within `max_age`. Host checks run
a command over ssh and compare stdout to a regex (or expect nothing). The
result replaces the `health` section of briefing.md --- one line when all is
well --- and a problem that was not in the previous run's state gets a
Pushover page, so a job that stops firing is noticed the day it happens
rather than when someone reads the briefing. Config format:

    [[unit]]
    name = "aps-scrape"          # <name>.service and <name>.timer
    max_age = "26h"              # last completion within this (m/h/d)

    [[ssh]]
    host = "strproxy"
    [[ssh.check]]
    name = "proxy answers"
    command = "curl -s -o /dev/null -w '%{http_code}' -m 5 http://localhost:4000"
    expect = "^[0-9]{3}$"        # regex against stdout; or expect_empty = true
    ignore = ["logrotate"]       # lines dropped before an expect_empty check
"""

from __future__ import annotations

import json
import re
import subprocess
import sys
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from pathlib import Path

import tomllib

from pkb_tools import briefing
from pkb_tools.notebook import state_dir

UNITS = {"m": 60, "h": 3600, "d": 86400}


@dataclass(frozen=True)
class Problem:
    key: str
    message: str


@dataclass(frozen=True)
class UnitStatus:
    timer_active: bool
    result: str
    active_state: str
    last_exit: datetime | None
    last_trigger: datetime | None


@dataclass(frozen=True)
class SshCheck:
    name: str
    command: str
    expect: str = ""
    expect_empty: bool = False
    ignore: tuple[str, ...] = ()


@dataclass(frozen=True)
class Config:
    units: dict[str, timedelta] = field(default_factory=dict)
    hosts: dict[str, tuple[SshCheck, ...]] = field(default_factory=dict)


def parse_duration(text: str) -> timedelta:
    match = re.fullmatch(r"(\d+)([mhd])", text.strip())
    if not match:
        raise ValueError(f"bad duration {text!r} (want e.g. 90m, 26h, 2d)")
    return timedelta(seconds=int(match.group(1)) * UNITS[match.group(2)])


def load_config(path: Path) -> Config:
    data = tomllib.loads(path.read_text())
    units = {u["name"]: parse_duration(u["max_age"]) for u in data.get("unit", [])}
    hosts = {
        h["host"]: tuple(
            SshCheck(
                name=c["name"],
                command=c["command"],
                expect=c.get("expect", ""),
                expect_empty=bool(c.get("expect_empty", False)),
                ignore=tuple(c.get("ignore", ())),
            )
            for c in h.get("check", [])
        )
        for h in data.get("ssh", [])
    }
    return Config(units=units, hosts=hosts)


def parse_systemd_time(text: str) -> datetime | None:
    """'Mon 2026-09-07 07:08:24 AEST' -> aware local datetime; '' -> None."""
    parts = text.split()
    if len(parts) < 3:
        return None
    return datetime.strptime(f"{parts[1]} {parts[2]}", "%Y-%m-%d %H:%M:%S").astimezone()


def systemctl_show(unit: str, *properties: str) -> dict[str, str]:
    args = ["systemctl", "--user", "show", unit]
    for prop in properties:
        args += ["-p", prop]
    out = subprocess.run(args, capture_output=True, text=True, check=False).stdout
    return dict(line.split("=", 1) for line in out.splitlines() if "=" in line)


def unit_status(name: str) -> UnitStatus:
    service = systemctl_show(
        f"{name}.service", "Result", "ActiveState", "ExecMainExitTimestamp"
    )
    timer = systemctl_show(f"{name}.timer", "ActiveState", "LastTriggerUSec")
    return UnitStatus(
        timer_active=timer.get("ActiveState") == "active",
        result=service.get("Result", "unknown"),
        active_state=service.get("ActiveState", "unknown"),
        last_exit=parse_systemd_time(service.get("ExecMainExitTimestamp", "")),
        last_trigger=parse_systemd_time(timer.get("LastTriggerUSec", "")),
    )


def evaluate_unit(
    name: str, max_age: timedelta, status: UnitStatus, now: datetime
) -> list[Problem]:
    problems: list[Problem] = []
    if not status.timer_active:
        problems.append(
            Problem(
                f"{name}:timer",
                f"**{name}**: timer is not active, so it will never fire",
            )
        )
    if status.active_state == "failed" or status.result not in (
        "success",
        "unknown",
        "",
    ):
        problems.append(
            Problem(
                f"{name}:failed",
                f"**{name}**: last run failed (result={status.result})",
            )
        )
    if status.active_state == "activating":
        return problems  # mid-run: age is meaningless until it finishes
    last = status.last_exit or status.last_trigger
    if last is None:
        problems.append(
            Problem(f"{name}:never", f"**{name}**: has never run on this machine")
        )
    elif now - last > max_age:
        age = now - last
        problems.append(
            Problem(
                f"{name}:stale",
                f"**{name}**: last completed {last:%a %H:%M}, {age.total_seconds() / 3600:.0f}h ago (limit {max_age.total_seconds() / 3600:.0f}h)",
            )
        )
    return problems


def run_ssh(host: str, command: str) -> tuple[int, str]:
    result = subprocess.run(
        ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=15", host, command],
        capture_output=True,
        text=True,
        stdin=subprocess.DEVNULL,
        timeout=60,
        check=False,
    )
    return result.returncode, result.stdout


def evaluate_ssh(host: str, check: SshCheck, rc: int, stdout: str) -> Problem | None:
    key = f"{host}:{check.name}"
    lines = [line for line in stdout.strip().splitlines() if line.strip()]
    if check.expect_empty:
        kept = [
            line for line in lines if not any(re.search(p, line) for p in check.ignore)
        ]
        if kept:
            return Problem(key, f"**{host}** {check.name}: " + "; ".join(kept[:5]))
        return None
    if rc != 0 and not lines:
        return Problem(
            key, f"**{host}** {check.name}: command exited {rc} with no output"
        )
    if check.expect and not re.search(check.expect, stdout.strip()):
        return Problem(key, f"**{host}** {check.name}: got {stdout.strip()[:80]!r}")
    return None


def check_host(host: str, checks: tuple[SshCheck, ...]) -> list[Problem]:
    rc, _ = run_ssh(host, "true")
    if rc != 0:
        return [Problem(f"{host}:ssh", f"**{host}**: unreachable over ssh (exit {rc})")]
    problems = []
    for check in checks:
        problem = evaluate_ssh(host, check, *run_ssh(host, check.command))
        if problem:
            problems.append(problem)
    return problems


def render(problems: list[Problem], n_units: int, n_checks: int) -> str:
    if not problems:
        return f"All {n_units} watched jobs ran on time and all {n_checks} host checks passed."
    return f"{len(problems)} problem(s):\n\n" + "\n".join(
        f"- {p.message}" for p in problems
    )


def page_new_problems(problems: list[Problem]) -> None:
    """Page once per newly-seen problem key; state lives in the state dir."""
    path = state_dir() / "health.json"
    previous = set(json.loads(path.read_text())) if path.exists() else set()
    current = {p.key for p in problems}
    new = [p for p in problems if p.key not in previous]
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(sorted(current)))
    if not new:
        return
    body = "\n".join(re.sub(r"\*\*", "", p.message) for p in new)
    subprocess.run(
        ["mise", "exec", "--", "notify-pushover", "--title", "pkb-health"],
        input=body,
        text=True,
        capture_output=True,
        check=False,
    )


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    if len(args) != 1:
        print(__doc__, file=sys.stderr)
        return 2
    config = load_config(Path(args[0]))
    now = datetime.now().astimezone()
    problems: list[Problem] = []
    for name, max_age in config.units.items():
        problems += evaluate_unit(name, max_age, unit_status(name), now)
    for host, checks in config.hosts.items():
        problems += check_host(host, checks)
    n_checks = sum(len(c) for c in config.hosts.values())
    briefing.write_section("health", render(problems, len(config.units), n_checks))
    page_new_problems(problems)
    for problem in problems:
        print(re.sub(r"\*\*", "", problem.message))
    return 0


if __name__ == "__main__":
    sys.exit(main())
