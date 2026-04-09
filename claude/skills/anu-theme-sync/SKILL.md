---
name: anu-theme-sync
description:
  Manages astro-theme-anu and astromotion across all consuming projects. Use
  when syncing versions, propagating breaking changes, checking dependency
  status, testing consumers, or managing local dev links. Triggers on "sync
  theme", "update theme", "check consumers", "theme status", or when working
  across multiple astro-theme-anu consuming projects.
---

You manage the projects that consume astro-theme-anu and/or astromotion. Your
job is to keep them in sync, propagate changes, and catch breakage early.

## The dependency chain

These three packages form a tight dependency chain where changes ripple through:

```
astromotion (GitHub: benswift/astromotion)
    ↓
astro-theme-anu (~/projects/astro-theme-anu/packages/astro-theme-anu)
    ↓                    ↓
    ↓        has astromotion as optional peer dep
    ↓
┌───────────────────────────────────────────┐
│ Consumers                                 │
│                                           │
│ comp4020-agentic-coding-studio            │
│   theme via git+https (ANU GitLab)        │
│                                           │
│ teaching-archive (pnpm monorepo)          │
│   theme via git submodule + file: override│
│   astromotion via pnpm catalog            │
│   sites: comp1720, comp2300, comp2710,    │
│          compiot-bit, extn1019, landing    │
│                                           │
│ astro-theme-anu/docs (workspace:*)        │
│   dogfoods the theme                      │
└───────────────────────────────────────────┘
```

The most common pain point is coordinating changes across this chain --- a
change in the theme or astromotion needs to land in both comp4020 and
teaching-archive, which have different dependency mechanisms.

## Discovering consumers

Discover consumers dynamically rather than relying on the diagram above (which
may be stale). Scan `~/projects/` for `package.json` files that reference
`astro-theme-anu` or `astromotion`:

```bash
grep -rl '"astro-theme-anu"\|"astromotion"' ~/projects/*/package.json ~/projects/*/website/package.json ~/projects/*/sites/*/package.json 2>/dev/null
```

For each match, read the `package.json` to determine which packages it depends
on and the dependency format.

### Filtering

Skip non-consumers:

- `~/projects/astro-theme-anu/packages/astro-theme-anu/` (source, not consumer)
- Any path containing `/node_modules/`
- Any path under `~/projects/archive/`

Include the theme's docs site (`~/projects/astro-theme-anu/docs/`) --- it's a
valid consumer.

Template directories (`templates/base/`, `templates/course/`) are starters, not
deployed sites. Include in status checks but skip for build verification unless
asked.

## How each consumer gets its dependencies

Understanding the update mechanism for each consumer type is critical --- they
are all different.

### comp4020-agentic-coding-studio

- **Theme**: `git+https://gitlab.anu.edu.au/...#main&path:/packages/astro-theme-anu`
- **Update**: `pnpm update astro-theme-anu` re-resolves the git ref
- **Note**: this pulls from GitLab, not GitHub. The ANU GitLab mirror must be
  up to date. If the user pushes to GitHub only, this project won't see the
  change until the GitLab remote is also updated.

### teaching-archive

- **Theme**: git submodule at `submodules/astro-theme-anu/`, wired in via pnpm
  override `"astro-theme-anu": "file:./submodules/astro-theme-anu/packages/astro-theme-anu"`
  in the root `package.json`
- **Astromotion**: pinned in `pnpm-workspace.yaml` catalog as
  `github:benswift/astromotion`
- **Theme update**:
  ```bash
  cd ~/projects/teaching-archive/submodules/astro-theme-anu && git pull origin main
  cd ~/projects/teaching-archive && pnpm install
  ```
- **Astromotion update**:
  ```bash
  cd ~/projects/teaching-archive && pnpm update astromotion
  ```
- **Key detail**: individual sites declare `"astro-theme-anu": "*"` which
  resolves via the root override. You never need to touch individual site
  `package.json` files for version bumps.

### astro-theme-anu/docs

- **Theme**: `workspace:*` (always uses local source)
- **Astromotion**: `github:benswift/astromotion`
- **Update**: `cd ~/projects/astro-theme-anu && pnpm install` (theme is
  automatic; for astromotion: `pnpm update astromotion`)

### Other consumers (benswift-me, cyberneticstudio-xyz, llms-unplugged, etc.)

These typically only depend on astromotion via `github:benswift/astromotion`.
They're less tightly coupled --- update with `pnpm update astromotion`.

## Operations

### 1. Status check

1. Run the discovery scan
2. For each consumer, read `package.json` for declared refs
3. Run `pnpm ls astro-theme-anu astromotion --depth 0` for resolved versions
4. For teaching-archive, also check submodule status:
   `cd ~/projects/teaching-archive && git submodule status`

Present as a table:

```
| Project | Theme ref | Theme resolved | Astromotion ref | Astromotion resolved |
```

### 2. Update dependencies

Follow the consumer-specific procedures above. Order matters when both packages
changed:

1. Push astromotion changes to GitHub first
2. Push/update astro-theme-anu (which may depend on new astromotion)
3. Update consumers

For teaching-archive, always pull the submodule before running `pnpm install`.

### 3. Build verification

After updating, verify each consumer builds. Use parallel agents for independent
projects:

```bash
cd <project-path> && pnpm build
```

For teaching-archive, build specific sites or all:

```bash
cd ~/projects/teaching-archive && pnpm --filter <site-name> build
```

Report results as a table:

```
| Project | Build | Notes |
```

### 4. Breaking change propagation

1. **Identify the change** --- read the relevant commits in the source repo
2. **Discover consumers** --- run the discovery scan
3. **Assess impact** --- grep across all consumers for usage of the changed API
4. **Create a migration checklist** --- list each affected consumer and what
   needs changing
5. **Apply fixes** --- use parallel agents for independent projects
6. **Verify builds**

Common breaking changes:

- Component renames or removed exports
- CSS custom property changes (`--at-*`, `--anu-*` prefixes)
- Astro config API changes (integration options)
- Layout prop changes
- Content collection schema changes
- Astromotion preprocessor syntax (slide separators, directives, bg images)

### 5. Local development linking

For testing theme changes against a consumer before pushing:

```json
{
  "pnpm": {
    "overrides": {
      "astro-theme-anu": "file:/Users/ben/projects/astro-theme-anu/packages/astro-theme-anu"
    }
  }
}
```

Then `pnpm install`. Teaching-archive already uses this pattern permanently via
its submodule.

Always remind the user to remove temporary overrides before committing.

## Workflow principles

- **Parallel agents**: fan out across independent projects, don't visit them
  sequentially.
- **Fail fast**: report build failures immediately.
- **Minimise churn**: if only astromotion changed, skip theme-only consumers.
- **Respect dependency formats**: don't change a project's dep format (git+https
  to file:, etc.) unless asked. Each exists for a reason.
- **Git hygiene**: don't commit in consuming projects without asking.
- **Report, don't assume**: show what needs changing before applying fixes.
- **Check remotes**: remember comp4020 pulls from ANU GitLab, not GitHub. If the
  user only pushed to one remote, flag the other.
