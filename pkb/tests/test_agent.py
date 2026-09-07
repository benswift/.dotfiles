from __future__ import annotations

import subprocess
from pathlib import Path

import pytest
from pkb_tools import agent


def completed(
    stdout: str = "", stderr: str = "", rc: int = 0
) -> subprocess.CompletedProcess[str]:
    return subprocess.CompletedProcess(
        args=["agent-run"], returncode=rc, stdout=stdout, stderr=stderr
    )


def test_quota_message_is_a_failure(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    monkeypatch.setattr(
        agent.subprocess,
        "run",
        lambda *_a, **_k: completed("You've hit your weekly limit · resets 2am"),
    )
    with pytest.raises(agent.AgentFailure, match="refused"):
        agent.run_agent("p", cwd=tmp_path)


def test_nonzero_exit_is_a_failure(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    monkeypatch.setattr(
        agent.subprocess, "run", lambda *_a, **_k: completed(stderr="boom", rc=3)
    )
    with pytest.raises(agent.AgentFailure, match="exited 3"):
        agent.run_agent("p", cwd=tmp_path)


def test_success_writes_log(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(agent.subprocess, "run", lambda *_a, **_k: completed("done"))
    log = tmp_path / "logs" / "run.log"
    result = agent.run_agent("p", cwd=tmp_path, log=log)
    assert result.stdout == "done"
    assert log.read_text() == "done"


def test_command_carries_profile_and_model(tmp_path: Path) -> None:
    command = agent.build_command("hi", cwd=tmp_path, profile="codex-sub", model="gpt")
    assert command[:4] == ["agent-run", "--bypass-permissions", "--cwd", str(tmp_path)]
    assert "--profile" in command and "codex-sub" in command
    assert command[-1] == "hi"


def test_default_profile_is_left_to_agent_run(tmp_path: Path) -> None:
    assert "--profile" not in agent.build_command("hi", cwd=tmp_path)
