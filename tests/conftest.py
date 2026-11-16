"""Strip the test process's environment to what the tests and their tools need.

The caller's shell exports every secret in the mise env, and a failing test's
traceback prints the env it passed to a subprocess. Trimming os.environ before
any test module is imported, and before xdist starts its workers, keeps
secrets out of every copy a test makes of it.
"""

from __future__ import annotations

import os

import pytest

# What tests and the tools they run read: the basics of a login shell, uv's
# and git's own settings, CI's markers, and this repo's documented overrides.
KEPT = {"PATH", "HOME", "USER", "LOGNAME", "SHELL", "LANG", "LANGUAGE", "TERM"}
KEPT |= {"TMPDIR", "TZ", "CI", "GITHUB_ACTIONS"}
KEPT |= {"WIZ", "HELIX_SRC", "HELIX_PREFIX"}
KEPT_PREFIXES = ("LC_", "XDG_", "UV_", "PYTEST_")


def pytest_configure(config: pytest.Config) -> None:
    for name in list(os.environ):
        if name not in KEPT and not name.startswith(KEPT_PREFIXES):
            del os.environ[name]
