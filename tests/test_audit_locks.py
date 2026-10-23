"""Ensure the dependency scanner covers universal locks without installing them."""

import runpy
from pathlib import Path

import pytest

requirements = runpy.run_path(
    str(Path(__file__).resolve().parents[1] / "scripts/audit-locks.py")
)["requirements"]


def test_dependency_free_lock(tmp_path: Path) -> None:
    lock = tmp_path / "empty.lock"
    lock.write_text('version = 1\nrequires-python = ">=3.12"\n')
    assert requirements(lock) == ""


def test_all_platform_artifacts_are_audited(tmp_path: Path) -> None:
    lock = tmp_path / "uv.lock"
    lock.write_text("""version = 1
[[package]]
name = "local-project"
version = "0.1"
source = { editable = "." }
[[package]]
name = "example"
version = "1.2.3"
source = { registry = "https://pypi.org/simple" }
resolution-markers = ["sys_platform == 'win32'"]
sdist = { hash = "sha256:aaa" }
wheels = [{ hash = "sha256:bbb" }, { hash = "sha256:aaa" }]
""")
    assert requirements(lock) == "example==1.2.3 --hash=sha256:aaa --hash=sha256:bbb\n"


@pytest.mark.parametrize(
    "content",
    [
        "version = 2",
        """version = 1
[[package]]
name = "example"
version = "1.0"
source = { git = "https://example.invalid/repo" }
""",
        """version = 1
[[package]]
name = "example"
version = "1.0"
source = { registry = "https://pypi.org/simple" }
""",
    ],
)
def test_unverifiable_locks_fail_closed(tmp_path: Path, content: str) -> None:
    lock = tmp_path / "uv.lock"
    lock.write_text(content)
    with pytest.raises(ValueError):
        requirements(lock)
