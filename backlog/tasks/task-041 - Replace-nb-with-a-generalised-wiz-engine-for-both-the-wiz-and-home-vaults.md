---
id: TASK-041
title: Replace nb with a generalised wiz engine for both the lx and home vaults
status: To Do
assignee: []
created_date: '2026-10-28T09:55:00Z'
updated_date: '2026-10-29T09:00:00Z'
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
supplies its own types and workflows as files. The shared LX vault (`~/.nb/wiz`,
named `lx`) and Ben's private notebook (`~/.nb/home`, named `home`) both become
vaults of this engine, and nb, pkb-agent and pkb-todo retire.

This task is the design and the map of the whole cross-repo change. It is too
large to be Done in one go: before work starts, split it into child tasks, one
per step of the order of work below, each of which leaves every repo working on
its own. Cole and Rob have been briefed and support making wiz general purpose,
which is part of wiz's own mission ("work in other scenarios"); a multi-vault
future, with different types and workflows per vault, is the point.

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
  vault's config borrows the [mdbase](https://github.com/mdbase-dev/mdbase-spec)
  layout, because a config format is needed and theirs is fine. Wiz is
  mdbase-shaped, not mdbase-conformant: the spec is at 0.3 and carries
  lifecycle, data-contract and projection machinery no vault asks for, and
  chasing its fixtures would pull the engine towards features nobody uses.
  - `mdbase.yaml` at the root, with a format version
  - type definitions in `_types/`, each embedding a JSON Schema 2020-12, which
    is wiz's existing schema dialect
  - type matching by path glob only (wiz's folder-per-type; home becomes
    folder-per-type too, and the `note` catch-all is a lowest-precedence glob
    over the root). Frontmatter-rule matching is not needed by either vault.
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

### Naming

- "wiz" names the tool only: the binary, the engine repo (`wiz`) and the generic
  slash commands. "Vault" is the generic noun for a data repo. It's already
  Obsidian's word and wiz's own.
- Vaults are named for their contents, never "the wiz vault":
  - `home` is Ben's
  - the shared LX vault is named `lx`
- A vault's name is declared in its `mdbase.yaml` (`name: lx`), never derived
  from its folder. The name is used in the registry, cross-vault links
  (`[[lx:people/cole-cooney]]`) and `--all` output.
- Repos and folders carry a `wiz-` prefix, so Claude desktop (which shows the
  folder name above the session list) reads `wiz-lx`:
  - the GitLab project `u2548636/wiz` is renamed `u2548636/wiz-lx`, so a fresh
    `git clone` makes a `wiz-lx` folder with no instructions needed. GitLab
    redirects the old path, so existing clones keep working until their
    remotes are updated (have `wiz check` fix them)
  - home's repo and folder become `wiz-home`
- Local paths leave `~/.nb` once nb is gone: every vault lives in one
  top-level folder, `~/vaults/wiz-lx` and `~/vaults/wiz-home`. Cole and Rob
  have agreed to the churn.
- Avoid wizard-themed jargon ("spells" and the like): every extra term costs
  non-technical colleagues.

### Slash commands and skills

- Skills belong to the vault, as decision-12 says: they live in
  `.claude/skills/`, anyone may edit them, and the engine ships nothing into
  `.claude/`. The `/wiz` skill's triage rules and report layout are LX
  judgement, not generic text, and decision-6 puts judgement in skills. The
  engine's own instructions reach a session through `wiz brief` (below), which
  carries the command table, so a vault never copies generic text.
- Cole's and Rob's commands don't change: `/wiz` (the dancecard), `/wiz-email`,
  `/wiz-sync` and `/wiz-help`. Each vault's `/wiz` runs that vault's workflow
  named `dancecard`. On Ben's machines, `wiz run dancecard --all` merges every
  vault's dancecard, and home's own commands take whatever names Ben likes,
  since only he sees them.
- Open skills are a hygiene risk, not a security one: a skill can mislead
  Claude but can't widen its permissions, because `.claude/settings.json` is
  admin-only. The check is a command workflow on watson, outside the engine:
  over every vault Ben admins, `agent-run` reviews skills changed since the
  last run against a short rubric (calls only `wiz` and Git, doesn't route
  around the guards, doesn't tell Claude to touch protected paths) and files a
  task for Ben when one fails it.
- Shrink `ben:pkb` (a personal skill, so it appears in every session including
  LX ones) to what's genuinely Ben's (email, role hubs), so it doesn't compete
  with `/wiz`.

### CLAUDE.md per vault

- Each vault has its own root `CLAUDE.md`, holding only that vault's rules:
  record conventions, who changes what, and the Git policy.
- The generic wiz instructions live only in the engine. `wiz brief` prints
  them at session start (the SessionStart hook runs it, as now), along with the
  command table, the clone's state and the dancecard summary. Each vault's
  `CLAUDE.md` says to run `wiz brief` when it isn't already in context (after
  `/clear` or compaction, or where the hook didn't run). The binary always
  carries the instructions matching its own version, so nothing in any vault
  needs keeping in sync with it, and there is no generic text to copy into
  vaults.
- A session in one vault doesn't load another vault's `CLAUDE.md`. Writes
  through the CLI are still validated against the target vault's types, but
  its prose conventions aren't, so home's `CLAUDE.md` says to read
  `~/vaults/wiz-lx/CLAUDE.md` before writing to lx.

### No "current notebook"

- A machine-local registry at `~/.config/wiz/vaults.toml` maps names to paths
  (`home`, `lx`). On a collaborator's machine it has one entry or none, and the
  engine falls back to the vault containing the working directory.
- Every command takes explicit vault names or `--all`. Cross-vault links are
  written `[[lx:people/cole-cooney]]` and resolved through registry names.

### CLI is a contract

Outside scripts and agents drive the engine through the CLI. From the first
release it needs stable `--json` output and these commands:

- `query` (CEL filter, using cel-go)
- `show` and `path`
- `add`, `update` and `append`. Fields are set generically (a repeatable
  `--set key=value`, or JSON on stdin), never as a flag per field, so the CLI
  learns nothing about any type
- `rename`, which repairs links
- `validate`
- `run <workflow>`
- `due`, which runs whatever is due on this host
- `job <workflow>`, which asks another host to run a workflow (below)
- `sync`
- `guard`

### Commits and sync

- The invariant is that the engine never commits a file it wasn't asked to
  touch: a commit holds exactly the files of the write that made it (`rename`
  adds the files whose links it repaired; a workflow's writes land as one
  commit carrying the workflow's name). The engine never runs `git add --all`.
- Whether a write commits immediately is per-vault sync policy, not a
  universal rule. In lx it does, as now. In home it doesn't: twenty daily
  appends would be twenty commits, and the scheduled sync batches them anyway.
- Edits made in an editor stay uncommitted until something names them
  (`wiz sync <files>`, as now).
- Sync is per-vault policy:
  - home: a scheduled sync that commits dirty files that validate, leaves
    invalid ones uncommitted and lists them in the problems report, then pulls
    with rebase and pushes
  - lx: explicit sync, or through its skill, as now

### Workflows: exactly two kinds

A workflow is itself a typed record, validated like everything else:

```yaml
---
title: Morning job health check
trigger: { schedule: "0 7 * * *" } # or { manual: true }
host: weddle # optional; omit = any machine meeting `requires`
requires: [uv] # command workflows only; checked on PATH
timeout: 1h # command workflows only; default 1h
command: pkb-health data/health.toml # or run: x.star
---
```

A workflow is named by its file, so a manual trigger needs no name of its own.
The workflow type is defined by the engine, since the engine reads it, and is
closed: those fields and no others.

1. **Starlark workflows** (`run: *.star`, go.starlark.net) run anywhere,
   including managed and Windows laptops, with nothing to install, and they're
   sandboxed and pure over records. The sandbox is what matters: it's what
   lets colleagues and future vault owners write workflows, the same way
   decision-11 lets anyone write a Typst job because a Typst job can't run
   commands. Go does the loading, parsing, CEL filtering and backlink
   resolution; Starlark receives lists of dicts and returns text and/or record
   writes. Its API is roughly:
   - `ctx.query(cel)`, `ctx.get(path)`, `ctx.links(rec, field)`,
     `ctx.backlinks(rec)`
   - `ctx.today`, `ctx.me`, and the starlark-go `time` module
   - `ctx.add` / `ctx.update` / `ctx.append`

   There is **no exec, filesystem or network access**, no per-vault option to
   grant it, and no way for a Starlark workflow to invoke a command workflow.
   Add such a bridge only when a workflow needs it, with its own decision
   record. There is no Starlark test framework: the engine's Go tests run a
   `.star` file against a fixture vault and compare its output to a golden
   file, which is the harness the Go tests already have.

2. **Command workflows** (`command:`) run an external program, with `requires:`
   and optional `host:`. Where `requires` isn't met the workflow reports "needs
   typst, not available here" and is shown as unavailable, never failing
   confusingly. The contract with the program is small and the same for every
   command: it runs from the vault root, gets the record's body on stdin and
   `WIZ_VAULT`, `WIZ_VAULT_ROOT` and `WIZ_WORKFLOW` in its environment, and
   its exit code is the result.

   Scheduled agent tasks are not an engine concept. One is an ordinary command
   workflow whose command is `agent-run --model sonnet --expect-changes`, with
   `requires: [agent-run]` so it shows as unavailable on a machine without it,
   and whose body is the prompt, which `agent-run` reads from stdin. The
   engine never learns the words "prompt", "profile" or "model", its tests
   never mock an agent, and a change to how agents run is a dotfiles change,
   never an engine release. The pkb-agent rule that a run fails unless it
   changed the vault moves to `agent-run` as `--expect-changes` (non-zero
   when `git status --porcelain` in its cwd is empty afterwards), where it's
   runner-agnostic and vault-agnostic. The engine checks what an agent did the
   way it checks a human: the changed files are validated and guarded.

The two kinds map onto decision-12's two roles. Starlark workflows are open:
anyone may add or edit one, because it can't run code. Command workflows are
admin-only, because they can. The vault config states which path each kind
lives under, and the guard enforces it like `admin/tools/`.

Built-ins, not workflow kinds:

- **Scheduler:** `wiz due --all`, fired by one systemd timer on Linux and one
  launchd job on macOS, runs what is due for _this_ host across all registered
  vaults. `host:` replaces the "enable this timer on weddle ONLY" comments.
  The bookkeeping is generic and is the part of pkb-agent worth keeping:
  - run stamps are machine-local, and a run is stamped whether it failed or
    not, so a failing workflow waits for its next scheduled slot rather than
    retrying every poll
  - every run's output is captured to
    `~/.local/state/wiz/logs/<vault>/<workflow>/<timestamp>.log`, and the
    tail of it rides along in the failure
  - `timeout:` is enforced by the engine
  - before a batch, each vault is synced under its own policy, and a vault
    that won't sync has nothing run against it; after the batch it's synced
    again
  - `wiz due` exits non-zero naming each workflow that failed, which is what
    the unit's `OnFailure` turns into an oncall task
- **Requests:** `wiz job <workflow>` from a machine that isn't the workflow's
  host writes a request record and syncs it, and the host's next `wiz due`
  runs it and closes the request. This is today's `wiz job` (199 lines) kept,
  not a new mechanism: a laptop can't reach watson, so a record is the only
  trigger that works.
- **Sync.**

Outside scripts are just CLI users and aren't a wiz concept: `unit-oncall`, the
`launchd-run` failure path and shell functions.

### Updates and distribution

- Engine releases are tags in the engine repo. The release script pushes each
  platform build (macOS arm64, Windows amd64, Linux amd64) as a parentless
  commit on `refs/tools/<version>/<os>-<arch>` into the lx vault's remote, the
  one vault whose users install from a pin.
- Collaborators keep cloning one repo and installing over SSH with checksum
  verification, exactly as with today's `wiz-install`. They need no access to
  the engine repo.
- The lx vault pins its engine version (as `admin/tools/wiz-release.sha256`
  does now), and watson builds that pin. Home-only features stay dormant in
  the shared vault because types and workflows live in vaults, not the binary.
- Ben's machines don't install through vault pins. Two vaults with two pins
  and one `~/.wiz/bin/wiz` would have each vault's SessionStart hook
  installing its own pin over the other's. Instead, the helix pattern: a
  `wiz-build` script in dotfiles builds the engine from the rev in a
  `wiz/pinned-rev` file into `~/.local/bin`, `dotfiles doctor` checks it, and
  the engine's backward compatibility with older vault format versions is what
  lets that one build serve every vault. No mise entry, and no binaries pushed
  to home's remote.

### What leaves wiz's Go code

- `dancecard.go` (475 lines) becomes a Starlark workflow in the lx vault, with
  tasks by owner (the current user first), leads, coming up and mail. Port its
  `vault_test.go` coverage to golden-output tests over the `.star` file before
  deleting the Go.
- Mail cleaning (reply prefixes, quote-start detection, the Outlook
  first-time-sender banner) moves into `mail-export` on watson. It then writes
  mail _records_ with `thread`, `from`, `date` and a cleaned `summary`, so the
  dancecard's mail section becomes a query.
- Calendars become `event` records, written by one exporter for both vaults,
  and `calendar.go` is deleted. dotfiles' `calendar-archive` already does
  the hard part for home (feed as the skeleton, invites from the mail via mu
  for description, organiser and attendees, history frozen past the feed's
  reach), and it serves lx too. lx's `admin/tools/calendar-export` is deleted
  and `watson-run` loses its calendar step. The lx workflow stays on watson:
  watson already pulls the lxconvenor mailbox into `~/wiz-mail/lxconvenor`
  for `mail-export`, and mu is installed there (2026-10-29, Homebrew) with
  its store rooted at that Maildir, so the exporter runs unchanged and the
  lx calendar gets descriptions and attendees for the first time without
  weddle running anything for lx. `watson-run` (later the lx mail workflow)
  runs `mu index` after its mbsync pull, as weddle's `mu-index` unit does.
  Each vault has a command workflow (home: `command: calendar-archive`,
  `host: weddle`; lx: `command: calendar-archive --calendar lxconvenor`,
  `host: watson`; both `requires: [calendar-archive, mu]`) and the same
  `_types/event.md` by convention. Its field names follow the vault's
  schema.org rule and so match `delivery` wherever the two overlap:
  `title`, `start_date`, `end_date` (datetimes here, which schema.org
  allows), `location`, `organizer`, `attendee` (a list, singular like
  `performer`), `description`, `source`; plus `calendar`, `uid`, `all_day`
  and `busy`, invented because schema.org has no term. That means the
  exporter renames `start`, `end` and `attendees` when it starts writing
  records, and the vault renames nothing. Attendees resolve to people links
  where a `person` record's email matches, including cross-vault, so the
  dancecard's coming-up section and "who have I met with about X" are
  queries. `event` and `delivery` stay two types because they have two
  lifecycles: a delivery is curated (one record per occasion, dated by first
  enquiry, revenue once, enquiry to delivered), an event is generated (one
  per occurrence, feed-owned, rewritten every run, a three-day course being
  one delivery and three events). Merging them would mean the exporter
  clobbering curated records or field-level ownership rules. Four rules:
  the name is `event`, not `meeting`, because lectures, out-of-office blocks
  and deadlines are in the feeds too (a meeting is an event with attendees,
  which a query can say; a finer distinction is a field or tag, never a
  type); the exporter owns the whole record, so by decision-8 the link lives
  on the curated side, where it survives the next run: a note about a
  meeting has `about: [[events/…]]`, a delivery with a calendar hold links
  its event, and backlinks show the other direction. That turns
  `calendar.go`'s title-word heuristic for "this calendar line is a
  delivery" into a link, and gives a new check for free: calendar entries
  that look like deliveries with no delivery record, which is the enquiry
  nobody filed. The exporter writes the Markdown files directly then runs
  `wiz validate`, never `wiz add` per record, since home's archive is 7,475
  occurrences today and a process per record would take minutes. Only the
  live year churns; frozen history never changes. And lx re-types nothing
  for this: the imported meeting notes have no uid and predate the feed, so
  they stay notes with `about:` links; events begin where the feed does.
- `check.go` splits. Generic Git, SSH and engine checks stay in the engine; LX
  setup diagnosis moves into the `/wiz-help` skill's text, because it needs
  judgement, which decision-6 assigns to skills.
- `jobs.go` stays as the request built-in (`wiz job`), generalised from "a
  Typst file in `admin/jobs/`" to "a workflow with a `host:`". The deliveries
  report becomes a command workflow (`requires: [typst]`, `host: watson`) fed
  by `wiz query --json`.

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
  - the `#blocked` tag becomes `status: blocked`
  - `bot` and `oncall` remain tags. In lx `requested_by` is a people link, and
    a shared field name must keep its shared type
- **workflows:** home's current `tasks/` (pkb-agent definitions: `health.md`,
  `claude-retrospective.md`) become workflow records under the admin-only
  command-workflow path. `health.md` keeps `command: pkb-health
  data/health.toml` and its log reference moves to the engine's log path;
  `claude-retrospective.md` becomes `command: agent-run --model sonnet
  --expect-changes` with `requires: [agent-run]` and the prompt unchanged as
  its body. That frees `tasks/` for tasks.
- **dailies:**
  - `YYYYMMDD.md` with `# Daily YYYY-MM-DD` (176 files) become a `daily` type at
    `dailies/YYYY-MM-DD.md` with a `date` field
  - appending is an engine operation, `wiz append home daily "…"`
  - `.gitattributes` marks `dailies/** merge=union`, so concurrent appends from
    two machines merge cleanly
- **bookmarks:** `*.bookmark.md` (23 files) become a `bookmark` type in
  `bookmarks/` with `url`, `title`, `comment` and `tags`.
- **events:** `data/calendars/<calendar>.jsonl` (anu, phdconvenor, adirh;
  7,475 occurrences) becomes `event` records under `events/`, one per
  occurrence, written by `calendar-archive` as the command workflow described
  under "What leaves wiz's Go code". The JSONL files and the "Calendar
  archive" section of `CLAUDE.md` go; `wiz query` replaces `jq` and `rg`.
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

1. **New engine repo `wiz`:** first rename the GitLab project
   `u2548636/wiz` to `u2548636/wiz-lx` to free the name. Then create
   `u2548636/wiz` from `admin/tools/wiz` with its history preserved
   (`git filter-repo --subdirectory-filter`).
2. **`~/.nb/wiz`, the lx vault** (gitlab.anu.edu.au:u2548636/wiz):
   - convert `admin/tools/schemas/*.json` to `_types/` plus `mdbase.yaml`
   - add the dancecard and other Starlark workflows, and command workflows for
     watson's jobs, including the skills review, and for the lxconvenor
     calendar (`host: watson`, invites via mu over `~/wiz-mail/lxconvenor`)
   - add `_types/event.md` and `events/`, re-typing no existing record;
     delete `admin/tools/calendar-export` and `_generated/calendar.md`
   - update `wiz-install` and `wiz-update` to the new pin format, and update
     `watson-run` (minus its calendar step)
   - update `CLAUDE.md` and `start-here.md` (the commands list), and the `wiz`,
     `wiz-email`, `wiz-help` and `wiz-sync` skills, whose names don't change
   - in its `admin/backlog`, edit or replace decisions 3, 6, 9 and 11 in the
     same commits that override them (the vault's CLAUDE.md requires this),
     extend decision-12 so open Starlark workflows and admin-only command
     workflows sit on its two sides, and add a decision for the engine/vault
     split and the two workflow kinds, saying that agents are not a kind
   - file the engine work as wiz backlog tasks
   - nothing reaches Cole and Rob until the release passes a round trip on Rob's
     Windows laptop and Cole's Mac (the task-1 pattern)
3. **`~/.nb/home`:** the migration above, and a rewritten `CLAUDE.md`
   (structure, templates, tasks, dailies, sync and the CLI reference all
   change).
4. **`~/projects/claude-plugin-personal`:** rewrite the `ben:pkb` skill
   (`SKILL.md`, `email.md`) for the wiz CLI, then `dotfiles update`.
5. **This dotfiles repo:**
   - **mise:** drop the `github:xwmx/nb` pin from `mise/config.toml`. The
     engine is built from source by a new `bin/wiz-build` at the rev in
     `wiz/pinned-rev`, modelled on `helix-build`.
   - **files to delete:**
     - `bin/nb`, `tests/test_nb.py` and the `nb/*.nb-plugin` files
     - the `nb_plugin_links` function in `lib/symlink-manifest.sh`
     - `systemd/user/nb-sync.{service,timer}` and
       `launchd/com.xwmx.nb-sync.plist`
     - `pkb-todo` and `pkb-agent`
   - **scheduler units:** add a `wiz-due` systemd service and timer and a
     launchd plist, both with `OnFailure`/oncall wiring like the units they
     replace, and both running under the mise environment (as pkb-agent's do
     now) so an `agent-run` command finds its keys.
   - **`bin/agent-run`:** read the prompt from stdin when no positional prompt
     is given, and add `--expect-changes`, which exits non-zero when
     `git status --porcelain` in the working directory is empty after the run.
     Both are runner-agnostic.
   - **`bin/calendar-archive`:** grow a `--calendar` selector and an
     `lxconvenor` entry (`LXCONVENOR_ICS_URL`, already in watson's mise env),
     write `event` records into the vault it's run from instead of JSONL
     with the field names renamed to the vault's (`start_date`, `end_date`,
     `attendee`), and resolve attendees to people links. Its systemd timer
     goes; `wiz due` runs it as each vault's workflow.
   - **pkb package:** delete it. `pkb-health` moves to `bin/pkb-health` as a
     `uv run --script` (add it to `extend-include` in `ruff.toml`) and runs as a
     command workflow. Drop the pkb steps from CI and the editable install from
     `install.sh` and `dotfiles update`.
   - **`bin/unit-oncall`, and the failure path in `bin/launchd-run`:** switch to
     `wiz add home task --set title=… --set tags=[bot,oncall] --set unit=<unit>`,
     close it with an `update` found by
     `wiz query home 'unit == "<unit>" && status == "open"'` (not by title), and
     `wiz sync home`. The home `task` type gains a `unit` field.
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
   - **`systemd/user/wiz-validate.service`:** replace it. watson's run becomes
     workflows with `host: watson`, and weddle's timer just runs `wiz due lx` on
     watson over ssh. weddle stays the alarm, with its oncall wiring intact.
   - **CI:** add the engine's test suite to `.github/workflows/security.yml` if
     it's vendored or called from here.
6. **watson:** registry entry, engine build, and `watson-run` reduced to
   `wiz due lx` (or equivalent).
7. **daysy and any other Mac:** registry, engine install, the launchd scheduler;
   the nb-sync plist unloaded and removed.

## Order of work (each step leaves everything working)

1. In the engine repo: the split, vault config loading, registry and multi-vault
   commands, the CLI contract with `--json`, and the commit-touched-files model.
   The lx vault converts to `_types/` + `mdbase.yaml` with identical validation
   results, and nb still serves home.
2. The workflow records, the command and prompt kinds, and the scheduler. This
   absorbs pkb-agent; at this point watson's jobs, home's health check and the
   retrospective run as workflows.
3. Starlark: the host, API, golden tests and dancecard port, and the mail and
   calendar exports writing records. Nothing after this step depends on it:
   if it slips, steps 4 to 6 go ahead and the Go dancecard keeps serving lx.
4. The engine gains the daily, task-append and bookmark operations needed by
   home.
5. Home migration, run as a dry run, reviewed, then applied.
6. The dotfiles cutover, the skill rewrite, and nb's retirement on every host.
7. The collaborator release: pin the release in the lx vault only after the
   Windows and Mac round trip, then tell Cole and Rob what changed.

Rollback until step 6: nb still reads both vaults, because records stay plain
Markdown. Keep the nb pin and units until home has been on the engine for a week
without trouble.

## Decisions made while planning

- **Watson:** it runs the engine version pinned in the lx vault, not `main`, so
  the shared vault's automation runs exactly what Cole and Rob run.
- **Installing the engine on Ben's machines:** a source build pinned in
  dotfiles (`bin/wiz-build`, `wiz/pinned-rev`), never a vault pin, for the
  reason given under distribution. No mise entry.
- **mdbase:** the layout only. Conformance, the fixtures and any upstream
  contribution are a possible future, not a target, and the decision record
  for the engine/vault split says so.
- **CEL in Starlark:** only through `ctx.query(cel_string)`, evaluated in Go.
  That keeps one query language with one evaluation site.
- **Starlark stays** (reviewed 2026-10-28 and challenged as a language for one
  workflow). It's kept because the multi-vault future needs per-vault
  workflows that colleagues and vault owners can write themselves, and the
  sandbox is what makes that safe to open up. What went instead: the
  Starlark-to-command bridge (`ctx.run`), the `*_test.star` framework, and
  `on_change` triggers, none of which a first-wave workflow needs. Add any of
  them with its own decision record when a workflow does.
- **Agents aren't an engine concept** (same review). The engine's job is the
  deterministic, testable, slow-to-change part; agent runners change with the
  provider of the month, prompts are prose colleagues should be able to edit,
  and the whole category is optional per vault. So a scheduled agent task is a
  plain command workflow, the `profile:`/`model:` fields and the
  changed-nothing rule left the engine for `agent-run`, and the engine checks
  an agent's work the way it checks a person's: by validating and guarding
  what changed. What stays engine-side for every workflow is the bookkeeping
  (stamps, logs, timeout, sync around a batch, exit code), because it's
  generic. The one thing given up is engine-rendered context in prompts,
  which the agent gets by calling `wiz query` itself.
- **Skills stay vault-owned** (same review). Engine-shipped generic skills, a
  `wiz skills` command and the vault-name prefix on commands (`/lx-email`)
  were dropped: the `/wiz` skill's content is LX judgement, decision-12 had
  just opened skills to colleagues, and the naming rule served only Ben. The
  review of colleagues' skill edits is a watson workflow, not an engine
  feature.
- **Events are records, generated** (added 2026-10-29, after
  `calendar-archive` landed in dotfiles). A record type over an asset file
  because events are what people, projects and tasks join on, and the join
  wants links and `wiz query`, not `jq`. Generated rather than hand-kept
  because the feed is the only honest source of cancellations and moves, and
  invites alone miss anything Ben created himself. One exporter for both
  vaults, each run on the host that already holds that vault's mail, so lx
  keeps running entirely on watson. Not merged with `delivery`, which is
  already the vault's curated schema.org Event: the two differ in lifecycle
  and provenance, not subject, and that is the test for a type. A type
  exists for a distinct lifecycle or provenance, never for a distinct
  subject; expressiveness comes from `about`, links, tags and queries;
  schema.org names the fields; and types are vault files, so adding one is
  a commit and the validator's unmatched-record report says when one is
  due. The engine stays type-agnostic so that remains true. The "base
  ontology" both vaults share (person, org, project, task, note, event) is
  a documented set of type files copied by convention, never inherited.
- **Done todos:** migrate all of them as `status: done`, taking the creation
  date for the filename from nb's timestamp id, so history stays queryable.

TASK-038 (nb-sync hanging in a DarkWake) becomes obsolete once step 6 lands,
though the same launchd concern applies to the new scheduler plist.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 The engine lives in its own repo and contains no LX-specific types, folders or logic; a vault's types, rules and workflows come only from files in that vault (mdbase.yaml + _types/ + admin/workflows/)
- [ ] #2 The lx vault validates with the same results under the new engine as under wiz v0.9.0 before any record changes
- [ ] #3 Every CLI command takes explicit vault names or --all, resolved via ~/.config/wiz/vaults.toml, with stable --json output; there is no current-notebook state
- [ ] #4 No engine commit ever holds a file the write that made it didn't touch (plus link-repaired files on rename); nothing runs git add --all; whether a write commits at once is per-vault policy
- [ ] #5 Starlark workflows run on macOS arm64, Linux amd64 and Windows amd64 with nothing installed and have no exec, filesystem or network access and no way to invoke a command workflow; anyone may add one, while command workflows are admin-only and the guard enforces the split
- [ ] #6 A command workflow whose requires: is unmet, or whose host: is another machine, reports itself unavailable rather than failing
- [ ] #7 One scheduler (wiz due --all, from one systemd timer on Linux and one launchd job on macOS) runs every due workflow for its host across all vaults, and a failed run on weddle files an oncall task through the unit's OnFailure wiring
- [ ] #8 The lx dancecard is a Starlark workflow whose output matches the Go version on the same vault and date, covered by golden-output tests in the engine's Go suite; dancecard.go and calendar.go are deleted
- [ ] #9 Home is migrated: no *.todo.md, *.bookmark.md, YYYYMMDD.md or .index files remain, every tags field is a YAML list, and wiz validate home reports no problems
- [ ] #10 Concurrent appends to the same daily from two machines sync without a conflict
- [ ] #11 Cole and Rob each complete a get-latest, edit and share round trip on their own machines with the new release before it is pinned in the lx vault, and their slash commands keep their names
- [ ] #12 The lx vault's decision records overridden by this change (3, 6, 9, 11) are edited or replaced in the commits that override them, decision-12 is extended for the two workflow kinds, and a new decision records the engine/vault split and the mdbase-layout-only choice
- [ ] #13 No dotfiles file, unit, plist, test, mise pin or doc still invokes or describes nb, pkb-agent or pkb-todo, and dotfiles doctor verifies the engine instead
- [ ] #14 The ben:pkb skill and both vaults' CLAUDE.md files describe the wiz CLI and the new record types, and an agent session in each vault can add a task, append to a daily and run a workflow by following them
- [ ] #15 The engine's source and tests contain no agent, prompt, profile or model concept; home's scheduled agent tasks run as plain command workflows via agent-run, with every run stamped, logged, timed out and failed through the same bookkeeping as any other workflow
- [ ] #16 Both vaults carry an `event` type with the same fields, named to match `delivery` where they overlap, filled by `calendar-archive` as a command workflow in each (home on weddle, lx on watson, reading the invites through watson's mu index of the lxconvenor Maildir); the lx dancecard's coming-up section and home's calendar questions are answered by `wiz query` over those records, attendees link to people records where an email matches, `admin/tools/calendar-export`, `_generated/calendar.md`, `data/calendars/*.jsonl` and the calendar-archive timer are gone, and a re-run against an unchanged calendar touches no file
<!-- AC:END -->
