#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///

from __future__ import annotations

import os
import subprocess
from pathlib import Path

SCRIPT = Path(__file__).parents[1] / "bin" / "zj-switch"


def write_executable(path: Path, body: str) -> None:
    path.write_text(f"#!/bin/sh\n{body}")
    path.chmod(0o755)


def test_dry_run_counts_claude_and_codex_agents(tmp_path: Path) -> None:
    fake_bin = tmp_path / "bin"
    fake_bin.mkdir()
    write_executable(
        fake_bin / "zellij",
        "printf 'mixed-session\\nclaude-session\\ncodex-session\\n'\n",
    )
    write_executable(
        fake_bin / "ps",
        """cat <<'EOF'
100 1 zellij --server /run/zellij/mixed-session
101 100 claude --dangerously-skip-permissions
102 100 codex --profile dotfiles
200 1 zellij --server /run/zellij/claude-session
201 200 claude
300 1 zellij --server /run/zellij/codex-session
301 300 codex --profile dotfiles
302 301 claude --print nested-task
EOF
""",
    )

    env = os.environ | {
        "PATH": f"{fake_bin}:{os.environ['PATH']}",
        "XDG_STATE_HOME": str(tmp_path / "state"),
    }
    result = subprocess.run(
        [SCRIPT, "--dry-run"],
        check=True,
        capture_output=True,
        env=env,
        text=True,
    )

    rows = {
        fields[0]: fields[1:]
        for line in result.stdout.splitlines()
        if (fields := line.split())
    }
    assert rows == {
        "mixed-session": ["2"],
        "claude-session": ["1"],
        "codex-session": ["1"],
    }


if __name__ == "__main__":
    import pytest

    raise SystemExit(pytest.main([__file__, "-v", "-n", "auto"]))
