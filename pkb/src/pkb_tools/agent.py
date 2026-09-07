"""Run a headless agent through agent-run, and notice when it did not work.

The provider is agent-run's business (its `default` profile, or the task's
`profile:`); this module owns the failure detection the runners lack. Two
silent modes were observed in the old runner's logs, both exiting 0: a
subscription quota message in place of any work ("You've hit your weekly
limit"), and a run that ended its turn early ("I'll continue once the scan
completes."). Neither leaves a notebook change, which is the check that
catches both regardless of runner.
"""

from __future__ import annotations

import os
import subprocess
from dataclasses import dataclass
from pathlib import Path

QUOTA_MARKERS = (
    "hit your weekly limit",
    "hit your limit",
    "usage limit",
    "rate limit",
    "you've reached your",
    "quota exceeded",
    "insufficient credits",
)


class AgentFailure(RuntimeError):
    """The agent run cannot be trusted to have done its job."""


@dataclass(frozen=True)
class AgentResult:
    stdout: str
    stderr: str
    returncode: int


def build_command(
    prompt: str, *, cwd: Path, profile: str = "", model: str = ""
) -> list[str]:
    command = ["agent-run", "--bypass-permissions", "--cwd", str(cwd)]
    if profile:
        command += ["--profile", profile]
    if model:
        command += ["--model", model]
    command.append(prompt)
    return command


def quota_marker(text: str) -> str | None:
    lowered = text.lower()
    return next((m for m in QUOTA_MARKERS if m in lowered), None)


def run_agent(
    prompt: str,
    *,
    cwd: Path,
    profile: str = "",
    model: str = "",
    timeout: int = 3600,
    log: Path | None = None,
) -> AgentResult:
    """Run the prompt to completion. Raises AgentFailure on any bad sign."""
    env = {k: v for k, v in os.environ.items() if k != "CLAUDECODE"}
    try:
        completed = subprocess.run(
            build_command(prompt, cwd=cwd, profile=profile, model=model),
            cwd=cwd,
            capture_output=True,
            text=True,
            stdin=subprocess.DEVNULL,
            env=env,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired as error:
        raise AgentFailure(f"timed out after {timeout}s") from error
    result = AgentResult(completed.stdout, completed.stderr, completed.returncode)
    if log is not None:
        log.parent.mkdir(parents=True, exist_ok=True)
        log.write_text(
            result.stdout
            + ("\n--- stderr ---\n" + result.stderr if result.stderr else "")
        )
    if result.returncode != 0:
        raise AgentFailure(
            f"agent-run exited {result.returncode}: {tail(result.stderr or result.stdout)}"
        )
    marker = quota_marker(result.stdout + result.stderr)
    if marker:
        raise AgentFailure(
            f"provider refused the run ({marker!r}): {tail(result.stdout)}"
        )
    return result


def tail(text: str, lines: int = 3) -> str:
    kept = [line for line in text.strip().splitlines() if line.strip()][-lines:]
    return " | ".join(kept)
