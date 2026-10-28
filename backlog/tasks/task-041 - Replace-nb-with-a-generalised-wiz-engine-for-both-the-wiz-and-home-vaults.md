---
id: TASK-041
title: Replace nb with a generalised wiz engine for both the wiz and home vaults
status: To Do
assignee: []
created_date: '2026-10-28T09:55:00Z'
labels:
  - pkb
  - wiz
dependencies: []
references:
  - /home/ben/.nb/wiz/admin/tools/wiz
  - /home/ben/.nb/wiz/admin/backlog/decisions
  - /home/ben/.nb/wiz/CLAUDE.md
  - /home/ben/.nb/home/CLAUDE.md
  - /home/ben/projects/claude-plugin-personal/skills/pkb
  - pkb/
  - bin/unit-oncall
  - bin/launchd-run
  - https://github.com/mdbase-dev/mdbase-spec
  - https://github.com/google/starlark-go
  - https://github.com/google/cel-go
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Replace nb with a generalised wiz: one Go engine that manages and validates
typed collections of Markdown records and the relationships between them, and
runs workflows over them, manually or on timers. Each collection ("vault")
supplies its own types and workflows as files. The shared LX vault (`~/.nb/wiz`)
and Ben's private notebook (`~/.nb/home`) both become vaults of this engine, and
nb, pkb-agent and pkb-todo retire.

This task is the whole cross-repo change. It's written to be executed on weddle
in one push, with every repo moved in lockstep. Cole and Rob have been briefed
and support making wiz general purpose, which is part of wiz's own mission
("work in other scenarios").

## Why

- nb's model fights a typed notebook. Its tags are line-based, so home writes
  `tags: #a #b`, which a real YAML parser reads as `tags: null`. Todos are a
  special file type with state in the heading. Before many ordinary commands it
  auto-commits everything (`[nb] Commit`), including mid-rebase conflict markers
  (wiz decision-9). Dailies need an `nb sync` before every write to dodge
  add/add conflicts. `nb edit` needs stdin closed or it hangs and
  double-appends. `.index` files have to be gitignored everywhere (wiz
  decision-3).
- wiz already has most of an engine: record loading, JSON Schema validation,
  link checking, path-based permissions, commit-the-named-files sync and host
  jobs (`admin/tools/wiz`, about 3,600 lines of Go). What makes it LX-specific
  is a hardcoded folder-to-type map and the dancecard.
- One record model across both vaults gives Ben a unified view (one query over
  both vaults' tasks), validation of home, and records that can move between
  vaults unchanged.

## Design decisions (settled)

### Engine and vault split

- **Engine:** the Go binary, in its own repo. It knows nothing about LX,
  deliveries or any particular types.
- **Vault:** a Git repo of records plus its own config, types and workflows. A
  vault's config follows the [mdbase](https://github.com/mdbase-dev/mdbase-spec)
  layout, adopting the file format but not depending on mdbase's tools:
  - `mdbase.yaml` at the root, with a format version
  - type definitions in `_types/`, each embedding a JSON Schema 2020-12, which
    is wiz's existing schema dialect
  - type matching by path glob (wiz's folder-per-type) or by frontmatter rule
  - Run mdbase's conformance fixtures against the Go engine where they apply, so
    wiz can become a third mdbase implementation.
- **Vault-level rules in config:**
  - `links_out: none`, which enforces wiz's one-way rule (home may link into
    wiz; wiz never links out)
  - which paths are records and which are assets (non-Markdown files that links
    may target and that are checked for existence)
  - identities and protected paths, generalising `admin/tools/identities` and
    `requiredRole`
  - the sync policy
  - `.gitignore`d paths are invisible to the engine
- **Backward compatibility:** the engine stays compatible with older vault
  format versions, so Ben can run the newest engine against every vault.

### No "current notebook"

- A machine-local registry at `~/.config/wiz/vaults.toml` maps names to paths
  (`home`, `wiz`). On a collaborator's machine it has one entry or none, and the
  engine falls back to the vault containing the working directory.
- Every command takes explicit vault names or `--all`. Cross-vault links are
  written `[[wiz:people/cole-cooney]]` and resolved through registry names.

### CLI is a contract

Outside scripts and agents drive the engine through the CLI. From the first
release it needs stable `--json` output and these commands:

- `query` (CEL filter, using cel-go)
- `show` and `path`
- `add`, `update` and `append`
- `rename`, which repairs links
- `validate`
- `run <workflow>`
- `due`, which runs whatever is due on this host
- `sync`
- `guard`

### Commits and sync

- Every engine write commits exactly the files it touched. `rename` also commits
  the files whose links it repaired. A workflow's writes land as one commit
  carrying the workflow's name. The engine never runs `git add --all`.
- Edits made in an editor stay uncommitted until something names them
  (`wiz sync <files>`, as now).
- Sync is per-vault policy:
  - home: a scheduled sync that commits dirty files that validate, leaves
    invalid ones uncommitted and lists them in the problems report, then pulls
    with rebase and pushes
  - wiz: explicit sync, or through its skill, as now

### Workflows: exactly two kinds

A workflow is itself a typed record, validated like everything else:

```yaml
---
title: Morning job health check
trigger: { schedule: "0 7 * * *" } # or { manual: health } or { on_change: "tasks/**" }
host: weddle # optional; omit = any machine meeting `requires`
requires: [uv] # command workflows only; checked on PATH
command: pkb-health data/health.toml # or run: admin/workflows/x.star
---
```

1. **Starlark workflows** (`run: *.star`, go.starlark.net) run anywhere,
   including managed and Windows laptops, with nothing to install. They're
   sandboxed and pure over records. Go does the loading, parsing, CEL filtering
   and backlink resolution; Starlark receives lists of dicts and returns text
   and/or record writes. Its API is roughly:
   - `ctx.query(cel)`, `ctx.get(path)`, `ctx.links(rec, field)`,
     `ctx.backlinks(rec)`
   - `ctx.today`, `ctx.me`, and the starlark-go `time` module
   - `ctx.add` / `ctx.update` / `ctx.append`
   - `ctx.run(name, **args)`

   There is **no exec, filesystem or network access**, and no per-vault option
   to grant it. `ctx.run` invokes a _declared_ command workflow by name, so host
   pinning, `requires` and admin-only rules still apply. Starlark tests are
   `*_test.star` files that the engine runs against fixture vaults.

2. **Command workflows** (`command:`) run an external program, with `requires:`
   and optional `host:`. Where `requires` isn't met the workflow reports "needs
   typst, not available here" and is shown as unavailable, never failing
   confusingly. Prompt workflows are command workflows whose command is
   `agent-run` (prompt in the body, an optional `profile:` and `model:`), the
   same semantics as pkb-agent prompt tasks today. A prompt task fails unless it
   changed the vault.

Built-ins, not workflow kinds:

- **Scheduler:** `wiz due --all`, fired by one systemd timer on Linux and one
  launchd job on macOS, runs what is due for _this_ host across all registered
  vaults. Run stamps are kept locally on each machine. `host:` replaces the
  "enable this timer on weddle ONLY" comments.
- **Sync.**

Outside scripts are just CLI users and aren't a wiz concept: `unit-oncall`, the
`launchd-run` failure path and shell functions. Workflow definitions live under
a protected path (`admin/workflows/`) that only admins change.

### Updates and distribution

- Engine releases are tags in the engine repo. The release script pushes each
  platform build (macOS arm64, Windows amd64, Linux amd64) as a parentless
  commit on `refs/tools/<version>/<os>-<arch>` into _every_ vault remote that
  pins engine versions.
- Collaborators keep cloning one repo and installing over SSH with checksum
  verification, exactly as with today's `wiz-install`. They need no access to
  the engine repo.
- Each vault pins its engine version (as `admin/tools/wiz-release.sha256` does
  now). Moving home's pin never touches wiz's. Home-only features stay dormant
  in the shared vault because types and workflows live in vaults, not the
  binary.
- watson currently rebuilds wiz from `main` (`admin/tools/watson-run`). After
  the split it builds the engine from the engine repo's `main` (or the pin ---
  decide and record it).

### What leaves wiz's Go code

- `dancecard.go` (475 lines) becomes a Starlark workflow in the wiz vault, with
  tasks by owner (the current user first), leads, coming up and mail. Port its
  `vault_test.go` coverage to `*_test.star` before deleting the Go.
- Mail cleaning (reply prefixes, quote-start detection, the Outlook
  first-time-sender banner) moves into `mail-export` on watson. It then writes
  mail _records_ with `thread`, `from`, `date` and a cleaned `summary`, so the
  dancecard's mail section becomes a query.
- `calendar-export` writes event records instead of `_generated/calendar.md`,
  and `calendar.go` is deleted.
- `check.go` splits. Generic Git, SSH and engine checks stay in the engine; LX
  setup diagnosis becomes a wiz-vault workflow or stays in `/wiz-help`.
- `wiz job` becomes "trigger a command workflow pinned to `host: watson`". The
  deliveries report becomes a command workflow (`requires: [typst]`,
  `host: watson`) fed by `wiz query --json`.

## Home vault migration (do it properly)

Write a one-off migration script outside both repos (Python via
`uv run --script`). It needs a dry-run mode that reports every change, and it
should commit in reviewable chunks, one per conversion. The target schema:

- **tags:** every `tags: #a #b` becomes `tags: [a, b]`, and every nb tag query
  becomes CEL.
- **tasks (formerly todos):**
  - `*.todo.md` (about 1,020 files, with `# [ ]`/`# [x]` heading state) become
    `tasks/YYYY-MM-DD-<slug>.md`, using the same `task` type and field names as
    wiz
  - fields: `status: open | blocked | done`, `due_date`, optional `agent`
  - the `#blocked` tag becomes `status: blocked`, and the `#bot` tag becomes
    `requested_by: automation`
  - `oncall` remains a tag
- **workflows:** home's current `tasks/` (pkb-agent definitions: `health.md`,
  `claude-retrospective.md`) become workflow records in `admin/workflows/`. That
  frees `tasks/` for tasks.
- **dailies:**
  - `YYYYMMDD.md` with `# Daily YYYY-MM-DD` (176 files) become a `daily` type at
    `dailies/YYYY-MM-DD.md` with a `date` field
  - appending is an engine operation, `wiz append home daily "…"`
  - `.gitattributes` marks `dailies/** merge=union`, so concurrent appends from
    two machines merge cleanly
- **bookmarks:** `*.bookmark.md` (23 files) become a `bookmark` type with `url`,
  `title`, `comment` and `tags`.
- **links:** all links become path-form and validated. Numeric-id links (for
  example `[[projects/16]]`) and ambiguous bare names are resolved or reported.
- **assets:** `docs/` typst sources, `data/`, `scripts/` and `images/` are
  assets: link-checkable, not records.
- **people:** home's `person` type keeps `role`, `uid` and `path` (wiz's doesn't
  have them). Field names shared with wiz (`title`, `tags`, `works_for`, …) stay
  identical. Each vault keeps its own type files; there's no type inheritance
  yet.
- **the long tail:** a permissive `note` type matches anything unclaimed at the
  root, so capture never requires classification. It guarantees only `title` and
  list-shaped `tags`.
- **removed:** `.index` files disappear.

Run `wiz validate home` until it's clean. A first trial in this session (wiz
v0.9.0 against copies of home's `people/`, `projects/` and `roles/`) reported
about 180 problems, mostly unknown `role`/`path`/`uid` fields and bare-name
links. The trial also showed the current validator silently skips folders it has
no type for, such as `roles/` and top-level files. The new engine must report
unmatched records unless a type such as `note` claims them.

## Repos and what changes in each

1. **New engine repo:** created from `~/.nb/wiz/admin/tools/wiz` with history
   preserved (`git filter-repo --subdirectory-filter`), hosted on
   gitlab.anu.edu.au beside `u2548636/wiz`. Picking the name is an open
   question.
2. **`~/.nb/wiz`** (gitlab.anu.edu.au:u2548636/wiz):
   - convert `admin/tools/schemas/*.json` to `_types/` plus `mdbase.yaml`
   - add the dancecard and other Starlark workflows, and command workflows for
     watson's jobs
   - update `wiz-install` and `wiz-update` to the new pin format, and update
     `watson-run`
   - update `CLAUDE.md` and `start-here.md` (the commands list), and the `wiz`,
     `wiz-email`, `wiz-help` and `wiz-sync` skills
   - in its `admin/backlog`, edit or replace decisions 3, 6, 9, 11 and 12 in the
     same commits that override them (wiz's CLAUDE.md requires this), and add a
     decision for the engine/vault split and the two workflow kinds
   - file the engine work as wiz backlog tasks
   - nothing reaches Cole and Rob until the release passes a round trip on Rob's
     Windows laptop and Cole's Mac (the task-1 pattern)
3. **`~/.nb/home`:** the migration above, and a rewritten `CLAUDE.md`
   (structure, templates, tasks, dailies, sync and the CLI reference all
   change).
4. **`~/projects/claude-plugin-personal`:** rewrite the `ben:pkb` skill
   (`SKILL.md`, `email.md`) for the wiz CLI, then `dotfiles update`.
5. **This dotfiles repo:**
   - **mise:** drop the `github:xwmx/nb` pin from `mise/config.toml`, and decide
     how the engine is installed on Ben's machines (a pinned release via mise's
     GitHub backend, or built from source like `helix-build`).
   - **files to delete:**
     - `bin/nb`, `tests/test_nb.py` and the `nb/*.nb-plugin` files
     - the `nb_plugin_links` function in `lib/symlink-manifest.sh`
     - `systemd/user/nb-sync.{service,timer}` and
       `launchd/com.xwmx.nb-sync.plist`
     - `pkb-todo` and `pkb-agent`
   - **scheduler units:** add a `wiz-due` systemd service and timer and a
     launchd plist, both with `OnFailure`/oncall wiring like the units they
     replace.
   - **pkb package:** `pkb-health` stays as a command workflow's program; decide
     whether `pkb/` shrinks to just that or the script moves to `bin/`.
   - **`bin/unit-oncall`, and the failure path in `bin/launchd-run`:** switch to
     `wiz add home task --tag oncall --requested-by automation …`, `wiz update`
     to close, and `wiz sync home`.
   - **`mail/utils` student-db:** `studentdb/__init__.py` resolves
     `data/student-db.json` with `nb show --path`. Switch it to
     `wiz path home:data/student-db.json` or a direct path.
   - **`zshrc`:** rewrite `daily()` and the `todo` alias.
   - **viewing plugins:** the `pdf` and `docx` plugins become command workflows
     in home (`requires: [pandoc, typst]`). `cpath`, `pager` and
     `lumis-highlight` become shell functions over `wiz path` and `ts-cat`.
   - **`dotfiles doctor`:** verify the engine install and pin instead of nb's.
   - **docs:** update `CLAUDE.md` (Utilities, the `nb` paragraph, the pkb
     bullets, the ts-cat note about `nb show`), `claude/CLAUDE.md` (Tools →
     Notes) and `pkb/README.md`.
   - **`systemd/user/wiz-validate.service`:** keep it, or replace it with the
     scheduler running watson's workflows.
   - **CI:** add the engine's test suite to `.github/workflows/security.yml` if
     it's vendored or called from here.
6. **watson:** registry entry, engine build, and `watson-run` reduced to
   `wiz due wiz` (or equivalent).
7. **daysy and any other Mac:** registry, engine install, the launchd scheduler;
   the nb-sync plist unloaded and removed.

## Order of work (each step leaves everything working)

1. In the engine repo: the split, vault config loading, registry and multi-vault
   commands, the CLI contract with `--json`, and the commit-touched-files model.
   The wiz vault converts to `_types/` + `mdbase.yaml` with identical validation
   results, and nb still serves home.
2. The workflow records, the command and prompt kinds, and the scheduler. This
   absorbs pkb-agent; at this point watson's jobs, home's health check and the
   retrospective run as workflows.
3. Starlark: the host, API, tests and dancecard port, and the mail and calendar
   exports writing records.
4. The engine gains the daily, task-append and bookmark operations needed by
   home.
5. Home migration, run as a dry run, reviewed, then applied.
6. The dotfiles cutover, the skill rewrite, and nb's retirement on every host.
7. The collaborator release: pin the release in the wiz vault only after the
   Windows and Mac round trip, then tell Cole and Rob what changed.

Rollback until step 6: nb still reads both vaults, because records stay plain
Markdown. Keep the nb pin and units until home has been on the engine for a week
without trouble.

## Open questions to settle at the start

- the engine repo's name, and whether the binary stays `wiz`
- whether watson builds the engine from `main` or the pin
- how the engine is installed on Ben's machines
- which mdbase conformance profile to target
- whether CEL is exposed to Starlark directly or only through `ctx.query`
- how `on_change` triggers fire (a post-merge hook, or a check at sync time)

TASK-038 (nb-sync hanging in a DarkWake) becomes obsolete once step 6 lands,
though the same launchd concern applies to the new scheduler plist.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 The engine lives in its own repo and contains no LX-specific types, folders or logic; a vault's types, rules and workflows come only from files in that vault (mdbase.yaml + _types/ + admin/workflows/)
- [ ] #2 The wiz vault validates with the same results under the new engine as under wiz v0.9.0 before any record changes
- [ ] #3 Every CLI command takes explicit vault names or --all, resolved via ~/.config/wiz/vaults.toml, with stable --json output; there is no current-notebook state
- [ ] #4 Engine writes commit exactly the files they touched (plus link-repaired files on rename); nothing runs git add --all
- [ ] #5 Starlark workflows run on macOS arm64, Linux amd64 and Windows amd64 with nothing installed, have no exec/filesystem/network access, and reach external programs only through ctx.run on a declared command workflow
- [ ] #6 A command workflow whose requires: is unmet, or whose host: is another machine, reports itself unavailable rather than failing
- [ ] #7 One scheduler (wiz due --all, from one systemd timer on Linux and one launchd job on macOS) runs every due workflow for its host across all vaults, and a failed run files an oncall task
- [ ] #8 The wiz dancecard is a Starlark workflow whose output matches the Go version on the same vault and date, with its test coverage ported to *_test.star; dancecard.go and calendar.go are deleted
- [ ] #9 Home is migrated: no *.todo.md, *.bookmark.md, YYYYMMDD.md or .index files remain, every tags field is a YAML list, and wiz validate home reports no problems
- [ ] #10 Concurrent appends to the same daily from two machines sync without a conflict
- [ ] #11 Cole and Rob each complete a get-latest, edit and share round trip on their own machines with the new release before it is pinned in the wiz vault
- [ ] #12 Wiz's decision records overridden by this change (3, 6, 9, 11, 12) are edited or replaced in the commits that override them, and a new decision records the engine/vault split
- [ ] #13 No dotfiles file, unit, plist, test, mise pin or doc still invokes or describes nb, pkb-agent or pkb-todo, and dotfiles doctor verifies the engine instead
- [ ] #14 The ben:pkb skill and both vaults' CLAUDE.md files describe the wiz CLI and the new record types, and an agent session in each vault can add a task, append to a daily and run a workflow by following them
<!-- AC:END -->
