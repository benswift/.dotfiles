#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""The settings merge: lib/settings-merge.jq on its own, then create_symlinks.sh
merging the repo's real layers into a temporary home."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path
from typing import Any

import pytest

REPO = Path(__file__).parent.parent
FILTER = REPO / "lib" / "settings-merge.jq"

type Json = dict[str, Any]


def merge(tmp_path: Path, live: Json, enforced: Json, seed: Json | None) -> Json:
    (tmp_path / "enforced.json").write_text(json.dumps(enforced))
    seed_args = ["--argjson", "seed", "[]"]
    if seed is not None:
        (tmp_path / "seed.json").write_text(json.dumps(seed))
        seed_args = ["--slurpfile", "seed", str(tmp_path / "seed.json")]
    out = subprocess.run(
        [
            "jq",
            "-f",
            str(FILTER),
            "--slurpfile",
            "enforced",
            str(tmp_path / "enforced.json"),
            *seed_args,
        ],
        input=json.dumps(live),
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    return json.loads(out)


def test_enforced_key_replaces_the_live_value_wholesale(tmp_path: Path) -> None:
    live = {"hooks": {"Stop": ["old"], "SessionEnd": ["retired"]}}
    result = merge(tmp_path, live, {"hooks": {"Stop": ["new"]}}, None)
    assert result == {"hooks": {"Stop": ["new"]}}


def test_enforced_null_deletes_the_key(tmp_path: Path) -> None:
    result = merge(tmp_path, {"gone": 1, "kept": 2}, {"gone": None}, None)
    assert result == {"kept": 2}


def test_seed_fills_only_absent_keys(tmp_path: Path) -> None:
    live = {"model": "opus"}
    result = merge(tmp_path, live, {}, {"model": "sonnet", "tui": "fullscreen"})
    assert result == {"model": "opus", "tui": "fullscreen"}


def test_machine_keys_survive(tmp_path: Path) -> None:
    live = {"trustedWorkspaces": ["/x"], "a": 0}
    result = merge(tmp_path, live, {"a": 1}, {"b": 2})
    assert result == {"trustedWorkspaces": ["/x"], "a": 1, "b": 2}


def test_schema_is_never_written(tmp_path: Path) -> None:
    result = merge(tmp_path, {}, {"$schema": "e", "a": 1}, {"$schema": "s"})
    assert result == {"a": 1}


def manifest_merges() -> list[tuple[str, str]]:
    script = f'source "{REPO}/lib/symlink-manifest.sh"; printf "%s\\n" "${{JSON_MERGE_MANIFEST[@]}}"'
    out = subprocess.run(
        ["bash", "-c", script],
        env={**os.environ, "DOTFILES_DIR": str(REPO), "HOME": "/home/t"},
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    return [
        (source, target)
        for source, _, target in (line.partition(":") for line in out.splitlines())
    ]


def layers(source: str) -> tuple[Json, Json]:
    enforced_path = REPO / source
    seed_path = enforced_path.with_suffix(".seed.json")
    enforced = json.loads(enforced_path.read_text())
    seed = json.loads(seed_path.read_text()) if seed_path.exists() else {}
    for layer in (enforced, seed):
        layer.pop("$schema", None)
    return enforced, seed


def test_every_layer_pair_is_disjoint() -> None:
    for source, _ in manifest_merges():
        enforced, seed = layers(source)
        assert not enforced.keys() & seed.keys(), source


def test_create_symlinks_merges_into_a_live_file(tmp_path: Path) -> None:
    home = tmp_path / "home"
    targets = {
        source: Path(target.replace("~", str(home), 1))
        for source, target in manifest_merges()
    }
    for source, target in targets.items():
        enforced, seed = layers(source)
        target.parent.mkdir(parents=True, exist_ok=True)
        live: Json = {"machineOnly": True}
        if enforced:
            live[next(iter(enforced))] = "stale"
        if seed:
            live[next(iter(seed))] = "local choice"
        target.write_text(json.dumps(live))

    env = {**os.environ, "HOME": str(home), "DOTFILES_DIR": str(REPO)}
    run = [str(REPO / "create_symlinks.sh")]
    subprocess.run(run, env=env, capture_output=True, check=True)
    first = {source: target.read_text() for source, target in targets.items()}
    subprocess.run(run, env=env, capture_output=True, check=True)

    for source, target in targets.items():
        enforced, seed = layers(source)
        result = json.loads(target.read_text())
        assert result["machineOnly"] is True
        assert {k: result.get(k) for k in enforced if enforced[k] is not None} == {
            k: v for k, v in enforced.items() if v is not None
        }
        if seed:
            assert result[next(iter(seed))] == "local choice"
            assert all(k in result for k in seed)
        assert oct(target.stat().st_mode & 0o777) == "0o600"
        assert target.read_text() == first[source], "a second merge changed the file"


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-n", "auto", "-q"]))
