#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Tests for the shared agent-run dispatcher."""

from __future__ import annotations

import importlib.machinery
import importlib.util
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
        "CLAUDECODE": "1",
        "PATH": "/bin",
    }

    child = mod.resolve_environment(profile, environ)

    assert "ANTHROPIC_API_KEY" not in child
    assert "ANTHROPIC_BASE_URL" not in child
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
        codex_sandbox="",
        environ={"CLAUDE_BIN": "/opt/claude"},
    )

    assert command == [
        "/opt/claude",
        "--model",
        "deepseek-v4-flash",
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


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
