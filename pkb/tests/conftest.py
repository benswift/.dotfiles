from __future__ import annotations

import subprocess
from pathlib import Path

import pytest


@pytest.fixture
def notebook(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Path:
    """An empty git-backed notebook with tasks/, plus an isolated state dir."""
    nb_dir = tmp_path / "nb"
    (nb_dir / "tasks").mkdir(parents=True)
    subprocess.run(["git", "init", "-q"], cwd=nb_dir, check=True)
    subprocess.run(
        [
            "git",
            "-c",
            "user.email=t@example.invalid",
            "-c",
            "user.name=t",
            "commit",
            "-q",
            "--allow-empty",
            "-m",
            "init",
        ],
        cwd=nb_dir,
        check=True,
    )
    monkeypatch.setenv("PKB_NOTEBOOK", str(nb_dir))
    monkeypatch.setenv("PKB_STATE_DIR", str(tmp_path / "state"))
    return nb_dir
