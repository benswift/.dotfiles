#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Tests for the shared agent-run dispatcher."""

from __future__ import annotations

import argparse
import importlib.machinery
import importlib.util
import os
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "agent-run"
_loader = importlib.machinery.SourceFileLoader("agent_run", str(SCRIPT))
_spec = importlib.util.spec_from_loader("agent_run", _loader, origin=str(SCRIPT))
assert _spec is not None and _spec.loader is not None
mod = importlib.util.module_from_spec(_spec)
sys.modules["agent_run"] = mod
_spec.loader.exec_module(mod)


def test_claude_subscription_clears_api_routing() -> None:
    profile = mod.Profile(
        name="claude-sub",
        runner="claude",
        unset_env=("ANTHROPIC_API_KEY", "ANTHROPIC_BASE_URL"),
    )
    environ = {
        "ANTHROPIC_API_KEY": "pay-as-you-go",
        "ANTHROPIC_BASE_URL": "https://proxy.invalid",
        "ANTHROPIC_MODEL": "machine-default",
        "CLAUDECODE": "1",
        "PATH": "/bin",
    }

    child = mod.resolve_environment(profile, environ)

    assert "ANTHROPIC_API_KEY" not in child
    assert "ANTHROPIC_BASE_URL" not in child
    assert child["ANTHROPIC_MODEL"] == "machine-default"
    assert "CLAUDECODE" not in child
    assert child["PATH"] == "/bin"
    assert environ["ANTHROPIC_API_KEY"] == "pay-as-you-go"


def test_deepseek_maps_admin_secret_without_storing_it() -> None:
    profile = mod.Profile(
        name="deepseek",
        runner="claude",
        env={"ANTHROPIC_BASE_URL": "https://api.deepseek.com/anthropic"},
        secret_env={"ANTHROPIC_API_KEY": "DEEPSEEK_API_TOKEN"},
    )

    child = mod.resolve_environment(profile, {"DEEPSEEK_API_TOKEN": "secret"})

    assert child["ANTHROPIC_API_KEY"] == "secret"
    assert child["ANTHROPIC_BASE_URL"] == "https://api.deepseek.com/anthropic"


def test_missing_profile_secret_fails_loudly() -> None:
    profile = mod.Profile(
        name="openrouter",
        runner="claude",
        secret_env={"ANTHROPIC_AUTH_TOKEN": "OPENROUTER_API_KEY"},
    )

    with pytest.raises(ValueError, match="OPENROUTER_API_KEY"):
        mod.resolve_environment(profile, {})


def test_claude_command_preserves_headless_settings(tmp_path: Path) -> None:
    command = mod.build_command(
        mod.Profile(name="deepseek", runner="claude"),
        prompt="tick",
        model="deepseek-v4-flash",
        cwd=tmp_path,
        claude_dangerously_skip_permissions=False,
        claude_disallowed_tools="AskUserQuestion",
        claude_effort="max",
        codex_sandbox="",
        environ={"CLAUDE_BIN": "/opt/claude"},
    )

    assert command == [
        "/opt/claude",
        "--model",
        "deepseek-v4-flash",
        "--effort",
        "max",
        "--print",
        "--disallowedTools=AskUserQuestion",
        "tick",
    ]


def test_codex_command_preserves_sprite_sandbox(tmp_path: Path) -> None:
    command = mod.build_command(
        mod.Profile(name="codex-sub", runner="codex"),
        prompt="tick",
        model="",
        cwd=tmp_path,
        claude_dangerously_skip_permissions=False,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="danger-full-access",
        environ={"CODEX_BIN": "/opt/codex"},
    )

    assert command == [
        "/opt/codex",
        "exec",
        "--sandbox",
        "danger-full-access",
        "-C",
        str(tmp_path),
        "tick",
    ]


def test_foreign_runner_options_fail_rather_than_being_ignored() -> None:
    args = argparse.Namespace(
        claude_dangerously_skip_permissions=True,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="",
    )

    with pytest.raises(ValueError, match="claude option"):
        mod.check_runner_options("codex", args)


def test_own_runner_options_are_accepted() -> None:
    args = argparse.Namespace(
        claude_dangerously_skip_permissions=False,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="workspace-write",
    )

    mod.check_runner_options("codex", args)


def test_bypass_permissions_translates_per_runner(tmp_path: Path) -> None:
    common = {
        "prompt": "tick",
        "model": "",
        "cwd": tmp_path,
        "claude_dangerously_skip_permissions": False,
        "claude_disallowed_tools": "",
        "claude_effort": "",
        "codex_sandbox": "",
        "environ": {},
        "bypass_permissions": True,
    }

    claude = mod.build_command(mod.Profile(name="c", runner="claude"), **common)
    codex = mod.build_command(mod.Profile(name="x", runner="codex"), **common)

    assert "--dangerously-skip-permissions" in claude
    assert codex[codex.index("--sandbox") + 1] == "danger-full-access"


def test_explicit_codex_sandbox_wins_over_bypass(tmp_path: Path) -> None:
    command = mod.build_command(
        mod.Profile(name="codex-sub", runner="codex"),
        prompt="tick",
        model="",
        cwd=tmp_path,
        claude_dangerously_skip_permissions=False,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="workspace-write",
        environ={},
        bypass_permissions=True,
    )

    assert command[command.index("--sandbox") + 1] == "workspace-write"


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))


def test_child_does_not_inherit_the_dispatcher_script_venv() -> None:
    profile = mod.Profile(name="claude-api", runner="claude")
    environ = {
        "VIRTUAL_ENV": "/home/x/.cache/uv/environments-v2/agent-run-abc",
        "PATH": "/home/x/.cache/uv/environments-v2/agent-run-abc/bin:/usr/bin:/bin",
    }

    child = mod.resolve_environment(profile, environ)

    assert "VIRTUAL_ENV" not in child
    assert child["PATH"] == "/usr/bin:/bin"
    assert environ["VIRTUAL_ENV"]  # caller untouched


def test_registry_default_profile(tmp_path: Path) -> None:
    config = tmp_path / "profiles.toml"
    config.write_text(
        'version = 1\ndefault = "codex-sub"\n'
        '[profiles.claude-sub]\nrunner = "claude"\n'
        '[profiles.codex-sub]\nrunner = "codex"\n'
    )
    profiles, default = mod.load_registry(config)
    assert default == "codex-sub"
    assert set(profiles) == {"claude-sub", "codex-sub"}


def test_registry_rejects_unknown_default(tmp_path: Path) -> None:
    config = tmp_path / "profiles.toml"
    config.write_text(
        'version = 1\ndefault = "missing"\n[profiles.claude-sub]\nrunner = "claude"\n'
    )
    with pytest.raises(ValueError, match="default profile"):
        mod.load_registry(config)


def test_main_falls_back_to_default_profile(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    config = tmp_path / "profiles.toml"
    config.write_text(
        'version = 1\ndefault = "codex-sub"\n[profiles.codex-sub]\nrunner = "codex"\n'
    )
    monkeypatch.delenv("AGENT_PROFILE", raising=False)
    seen: list[list[str]] = []

    def fake_run(command: list[str], **_kwargs: object) -> object:
        seen.append(command)
        return argparse.Namespace(returncode=0)

    monkeypatch.setattr(mod.subprocess, "run", fake_run)
    assert mod.main(["--config", str(config), "hello"]) == 0
    assert seen[0][:2] == ["codex", "exec"]


def _registry(tmp_path: Path) -> Path:
    config = tmp_path / "profiles.toml"
    config.write_text('version = 1\n[profiles.fake]\nrunner = "claude"\n')
    return config


def _fake_agent(tmp_path: Path, script: str) -> dict[str, str]:
    agent = tmp_path / "fake-claude"
    agent.write_text(f"#!/bin/sh\n{script}\n")
    agent.chmod(0o755)
    return {"CLAUDE_BIN": str(agent)}


def _repo(tmp_path: Path) -> Path:
    repo = tmp_path / "repo"
    repo.mkdir()
    for args in (["init", "-q"], ["commit", "-q", "--allow-empty", "-m", "init"]):
        subprocess.run(
            [
                "git",
                "-C",
                str(repo),
                "-c",
                "user.name=t",
                "-c",
                "user.email=t@x",
                *args,
            ],
            check=True,
        )
    return repo


def test_prompt_from_stdin_and_expect_changes(tmp_path: Path) -> None:
    repo = _repo(tmp_path)
    config = _registry(tmp_path)
    env = {
        **os.environ,
        **_fake_agent(
            tmp_path, 'for a; do last=$a; done; printf "%s" "$last" > got.txt'
        ),
    }

    def run(*extra: str, stdin: str = "") -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [
                str(SCRIPT),
                "--config",
                str(config),
                "--profile",
                "fake",
                "--cwd",
                str(repo),
                *extra,
            ],
            input=stdin,
            capture_output=True,
            text=True,
            env=env,
            check=False,
        )

    result = run("--expect-changes", stdin="Summarise the week.\n")
    assert result.returncode == 0, result.stderr
    assert (repo / "got.txt").read_text() == "Summarise the week.\n"

    # The same run again writes the same file: nothing changed, so it fails.
    subprocess.run(["git", "-C", str(repo), "add", "."], check=True)
    subprocess.run(
        [
            "git",
            "-C",
            str(repo),
            "-c",
            "user.name=t",
            "-c",
            "user.email=t@x",
            "commit",
            "-qm",
            "x",
        ],
        check=True,
    )
    result = run("--expect-changes", stdin="Summarise the week.\n")
    assert result.returncode == 1
    assert "changed nothing" in result.stderr

    # Without the flag the same no-op run succeeds, and an empty prompt fails.
    assert run(stdin="Summarise the week.\n").returncode == 0
    assert "no prompt supplied" in run(stdin="  \n").stderr
