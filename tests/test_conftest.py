#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""conftest.py keeps the caller's secrets out of a failing test's traceback."""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

CANARY = "canary-6f1d0c"
FAILING = """
import os
import subprocess


def test_fails():
    subprocess.run(["false"], env={**os.environ}, check=True)
"""


def run_failing_test(tmp_path: Path, with_conftest: bool) -> str:
    (tmp_path / "test_fails.py").write_text(FAILING)
    if with_conftest:
        shutil.copy(Path(__file__).with_name("conftest.py"), tmp_path)
    result = subprocess.run(
        [sys.executable, "-m", "pytest", "-p", "no:cacheprovider", str(tmp_path)],
        # First, since pytest shortens a long env's repr from the end.
        env={"SOME_API_KEY": CANARY, **os.environ},
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 1, result.stdout
    return result.stdout


@pytest.mark.parametrize("with_conftest", [False, True])
def test_a_failing_tests_traceback_shows_no_secret(
    tmp_path: Path, with_conftest: bool
) -> None:
    out = run_failing_test(tmp_path, with_conftest)
    # Without conftest.py the secret leaks, which shows the check can see it.
    assert (CANARY in out) is not with_conftest


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-q", "-n", "auto"]))
