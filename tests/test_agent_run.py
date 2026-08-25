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


def test_grok_subscription_clears_api_billing_routes() -> None:
    profile = mod.Profile(
        name="grok-sub",
        runner="grok",
        unset_env=(
            "XAI_API_KEY",
            "GROK_CODE_XAI_API_KEY",
            "GROK_AUTH_PROVIDER_ACCESS_TOKEN",
            "GROK_AUTH_PROVIDER_COMMAND",
        ),
    )
    environ = {
        "XAI_API_KEY": "pay-as-you-go",
        "GROK_CODE_XAI_API_KEY": "also-pay-as-you-go",
        "GROK_AUTH_PROVIDER_COMMAND": "print-a-token",
        "PATH": "/bin",
    }

    child = mod.resolve_environment(profile, environ)

    assert "XAI_API_KEY" not in child
    assert "GROK_CODE_XAI_API_KEY" not in child
    assert "GROK_AUTH_PROVIDER_COMMAND" not in child
    assert child["PATH"] == "/bin"
    assert environ["XAI_API_KEY"] == "pay-as-you-go"


def test_grok_command_is_single_turn_headless(tmp_path: Path) -> None:
    command = mod.build_command(
        mod.Profile(name="grok-sub", runner="grok"),
        prompt="tick",
        model="grok-4.6",
        cwd=tmp_path,
        claude_dangerously_skip_permissions=False,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="",
        environ={"GROK_BIN": "/opt/grok"},
        grok_permission_mode="bypassPermissions",
        grok_effort="high",
        grok_output_format="json",
    )

    assert command == [
        "/opt/grok",
        "--model",
        "grok-4.6",
        "--permission-mode",
        "bypassPermissions",
        "--effort",
        "high",
        "--output-format",
        "json",
        "-p",
        "tick",
    ]


def test_grok_command_omits_unset_options(tmp_path: Path) -> None:
    command = mod.build_command(
        mod.Profile(name="grok-sub", runner="grok"),
        prompt="tick",
        model="",
        cwd=tmp_path,
        claude_dangerously_skip_permissions=False,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="",
        environ={},
    )

    assert command == ["grok", "-p", "tick"]


def test_foreign_runner_options_fail_rather_than_being_ignored() -> None:
    args = argparse.Namespace(
        claude_dangerously_skip_permissions=True,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="",
        grok_permission_mode="",
        grok_effort="",
        grok_output_format="",
    )

    with pytest.raises(ValueError, match="claude option"):
        mod.check_runner_options("grok", args)


def test_own_runner_options_are_accepted() -> None:
    args = argparse.Namespace(
        claude_dangerously_skip_permissions=False,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="",
        grok_permission_mode="bypassPermissions",
        grok_effort="high",
        grok_output_format="json",
    )

    mod.check_runner_options("grok", args)


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
    grok = mod.build_command(mod.Profile(name="g", runner="grok"), **common)

    assert "--dangerously-skip-permissions" in claude
    assert grok[grok.index("--permission-mode") + 1] == "bypassPermissions"


def test_bypass_permissions_refuses_codex(tmp_path: Path) -> None:
    with pytest.raises(ValueError, match="codex-sandbox"):
        mod.build_command(
            mod.Profile(name="codex-sub", runner="codex"),
            prompt="tick",
            model="",
            cwd=tmp_path,
            claude_dangerously_skip_permissions=False,
            claude_disallowed_tools="",
            claude_effort="",
            codex_sandbox="",
            environ={},
            bypass_permissions=True,
        )


def test_explicit_grok_permission_mode_wins_over_bypass(tmp_path: Path) -> None:
    command = mod.build_command(
        mod.Profile(name="grok-sub", runner="grok"),
        prompt="tick",
        model="",
        cwd=tmp_path,
        claude_dangerously_skip_permissions=False,
        claude_disallowed_tools="",
        claude_effort="",
        codex_sandbox="",
        environ={},
        grok_permission_mode="plan",
        bypass_permissions=True,
    )

    assert command[command.index("--permission-mode") + 1] == "plan"


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
