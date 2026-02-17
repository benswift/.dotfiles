# Python preference checklist

## Structural

| Check | Preference | Anti-pattern |
|---|---|---|
| Package manager | `uv` | poetry, pip, conda, pipenv |
| Project config | `pyproject.toml` (single source of truth) | setup.py, setup.cfg, requirements.txt |
| Python version | 3.12+ | anything below 3.12 |
| Project layout | `src/` layout for packages | flat layout with package at repo root |

## Soft

| Check | Preference | Anti-pattern |
|---|---|---|
| Type hints | comprehensive type annotations on all functions | untyped code, `Any` overuse |
| Data manipulation | polars | pandas |
| Validation | pydantic / sqlmodel | manual dict validation, marshmallow, attrs |
| HTTP client | httpx | requests, urllib3 |
| CLI framework | typer | argparse, click (directly) |
| Testing | pytest with parallel execution (`-n` flag) | unittest, nose, serial pytest |
| Linting/formatting | ruff (lint + format) | flake8, black, isort, pylint as separate tools |
| File paths | pathlib | os.path |
| Standalone scripts | uv inline script metadata (`# /// script`) | pip install into global env, requirements.txt for scripts |
| Logging | stdlib logging (INFO clean, DEBUG for diagnostics) | print statements in production code |
| Error handling | let exceptions bubble, catch at boundaries | bare except, swallowing exceptions in library code |
| Data structures | dataclasses or pydantic models | plain dicts for structured data |
| Async I/O | async/await with httpx for I/O-bound work | threads for I/O-bound work |
