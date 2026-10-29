#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# ///
"""Audit every tracked uv lock, including packages for other platforms.

Read locks directly so auditing never installs their packages or evaluates
project build hooks. Registry packages require hashes; unknown source types
fail closed. Editable/virtual local projects are covered by their dependencies.

Audits this repo, or the repo named by the one optional argument (the private
overlay's CI audits itself that way).
"""

from __future__ import annotations

import subprocess
import sys
import tempfile
import tomllib
from pathlib import Path


def requirements(path: Path) -> str:
    data = tomllib.loads(path.read_text())
    lines = []
    if data.get("version") != 1:
        raise ValueError(f"{path}: unsupported lock format")
    for package in data.get("package", []):
        source = package["source"]
        if "editable" in source or "virtual" in source:
            continue
        if "registry" not in source:
            raise ValueError(f"{path}: unsupported dependency source: {source}")
        artifacts = [*package.get("wheels", [])]
        if "sdist" in package:
            artifacts.append(package["sdist"])
        hashes = sorted({item["hash"] for item in artifacts})
        if not hashes:
            raise ValueError(f"{path}: {package['name']} has no artifact hashes")
        lines.append(
            f"{package['name']}=={package['version']} "
            + " ".join(f"--hash={value}" for value in hashes)
        )
    return "\n".join(lines) + "\n" if lines else ""


def main() -> int:
    root = (
        Path(sys.argv[1]).resolve()
        if len(sys.argv) > 1
        else Path(__file__).resolve().parents[1]
    )
    paths = (
        subprocess.check_output(["git", "ls-files", "-z", "*.lock"], cwd=root)
        .decode()
        .split("\0")
    )
    failed = False
    with tempfile.TemporaryDirectory(prefix="dotfiles-audit-") as directory:
        target = Path(directory) / "requirements.txt"
        for name in filter(None, paths):
            content = requirements(root / name)
            if not content:
                continue
            print(f"Auditing {name}", flush=True)
            target.write_text(content)
            result = subprocess.run(
                [
                    "uvx",
                    "pip-audit==2.10.1",
                    "--disable-pip",
                    "--require-hashes",
                    "-r",
                    str(target),
                ],
                check=False,
            )
            failed |= result.returncode != 0
    return int(failed)


if __name__ == "__main__":
    raise SystemExit(main())
