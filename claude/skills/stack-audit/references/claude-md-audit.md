# CLAUDE.md audit checklist

Verify that the project's CLAUDE.md (or equivalent agent instructions file)
accurately reflects the actual codebase. Divergence erodes trust in the
instructions and causes agents to make wrong assumptions.

## Checks

### File and path references

Scan CLAUDE.md for any file paths, directory references, or glob patterns. Verify
each one exists on disk. Common patterns to look for:

- explicit paths (`src/api/`, `config/database.yml`, `lib/my_app/`)
- at-prefixed paths used in dotfiles repos (`@zshrc`, `@helix/config.toml`)
- references to specific filenames (`pyproject.toml`, `mix.exs`, `Makefile`)
- directory structure descriptions ("the `utils/` folder contains...")

Report as divergence when a referenced path does not exist or has been renamed.

### Command and tool references

Scan for command invocations or tool names (e.g. `mix test`, `uv run pytest`,
`pnpm dev`). Cross-check:

- the tool is available (listed in `mise.toml`, `package.json` scripts,
  `Makefile` targets, or installed globally)
- the command syntax matches the project's actual setup (e.g. CLAUDE.md says
  `npm run` but project uses `pnpm`)

### Dependency and library references

When CLAUDE.md names specific libraries or frameworks (e.g. "uses Ash for
resources", "prefer polars over pandas"), verify:

- the library is in the project's dependency file (`pyproject.toml`, `mix.exs`,
  `package.json`)
- if CLAUDE.md says "don't use X", check that X is genuinely absent

### Structure and architecture claims

When CLAUDE.md describes the project layout, module organisation, or
architectural patterns, spot-check key claims:

- "the app uses Phoenix LiveView" --- verify LiveView is a dependency and
  LiveView modules exist
- "API routes are in `lib/my_app_web/controllers/`" --- verify the directory
  exists and contains controller modules
- "tests are in `tests/`" --- verify the directory name and structure

### Configuration and environment claims

When CLAUDE.md references specific configuration:

- database engine, cache backend, queue system --- verify against config files
- environment variable names --- verify they appear in the code or `.env.example`
- CI/CD setup --- verify against actual workflow files

### Staleness signals

Flag likely stale content:

- references to files in `.gitignore` or deleted files (check `git log` for
  recently removed paths mentioned in CLAUDE.md)
- instructions for features or workflows that no longer have corresponding code
- version numbers that don't match current dependency pins
- references to deprecated tools or libraries

## Report format

Use the same table format as other audit sections:

```
## CLAUDE.md accuracy

| Check | Status | Category | Notes |
|---|---|---|---|
| path references | aligned | soft | all 12 referenced paths exist |
| command `mix test` | divergence | soft | project uses `mix test.interactive` instead |
```

All CLAUDE.md accuracy checks are **soft** category --- they can be fixed freely
when requested without structural confirmation.
