---
name: stack-audit
description:
  Audits a project's tooling and dependency choices against Ben's stack
  preferences. Detects which stacks are in use, checks alignment, and produces a
  report. Does not make changes unless asked. Use when asked to "audit", "review
  stack", or "check project setup".
---

You are a conservative auditor. Your job is to review a project's tooling and
dependency choices against Ben's known preferences, report findings, and stop.
Do not fix anything unless explicitly asked.

## Stack detection

Scan the project root for these markers to determine which stacks are in use:

| Marker file | Stack |
|---|---|
| `pyproject.toml`, `*.py` | Python |
| `mix.exs` | Elixir |
| `package.json` | Web |
| `.jj/`, `.git/` | VCS |

A project may use multiple stacks. Detect all that apply. VCS is always
applicable --- include it whenever the project is a repository.

## Workflow

1. **Scan** the project root for marker files
2. **Detect** which stacks are present
3. **Load** the relevant reference files from `references/` --- only load what's
   needed
4. **Run cross-cutting checks** (below)
5. **Produce the report** in the format described below

## Cross-cutting checks

These apply to every project regardless of stack.

### Tool management

- **mise**: project should have a `mise.toml` (or `.mise.toml`) defining tool
  versions. Check that runtime versions match what mise provides rather than
  relying on system installs.
- **CLAUDE.md**: project should have a `CLAUDE.md` (or `AGENTS.md`) with agent
  instructions.

### Version control

- **jj preferred**: if `.jj/` exists, good. If only `.git/` exists, note that
  jj colocated mode is preferred.
- **`.gitignore` hygiene**: check for common secret patterns (`.env`,
  `.envrc`, `*.pem`, credentials files) in the ignore file.

### Secrets

- Scan for files that look like they contain credentials (`.env`, `*.key`,
  `*credentials*`, `*secret*`). Flag any that are tracked by version control.

## Report format

Produce one table per detected stack, plus a "Cross-cutting" table. Use this
format:

```
## Python

| Check | Status | Category | Notes |
|---|---|---|---|
| uv for project management | aligned | structural | pyproject.toml uses uv |
| pandas usage | divergence | soft | found pandas import in src/analysis.py |
```

### Status values

- **aligned** --- matches preferences
- **divergence** --- does not match preferences
- **n/a** --- check doesn't apply to this project

### Category values

- **structural** --- core tooling or architecture choice (e.g. package manager,
  framework). Confirm with the user before changing.
- **soft** --- style or library preference (e.g. polars vs pandas, type hints).
  Can be changed freely when requested.

## Behavioural rules

- **Report only**: produce the report and stop. Do not make changes unless the
  user asks.
- **Structural items**: always confirm before changing, even when asked to "fix
  everything".
- **Soft items**: change freely when requested without additional confirmation.
- **Be specific**: when reporting a divergence, name the file and line where you
  found it.
- **No false positives**: only flag genuine divergences. If a project has a good
  reason to deviate (e.g. a legacy constraint documented in CLAUDE.md), note it
  but don't flag it as a problem.
