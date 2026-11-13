# Ben's dotfiles repo

The (public) config files I use on my machines.

No raw credentials or passwords in this repo: it is public, and secrets live in
the untracked mise env ("Tool management" below).

Most `bin/` scripts carry a header comment explaining their own mechanism. This
file says where things live and which rules must not be broken; it does not
repeat what the scripts already document.

## Philosophy

- modern, zsh-only (no bash configs)
- cross-platform: macOS (Apple Silicon) and Linux
- mise-first for tool version management
- native package managers: Homebrew on macOS only, apt/dnf/etc on Linux (no
  Linuxbrew)
- easy to provision a new machine via `install.sh`

## Key commands

- `dotfiles` --- `doctor`, `update` and the rest (`dotfiles --help`).
  `dotfiles settings` lists keys a machine's merged agent settings hold beyond
  the repo's layers, to promote into one
- `./install.sh` --- bootstrap a new machine (also works via curl)
- `./create_symlinks.sh` --- create/update symlinks only

## Repo structure

Config files are symlinked into place (`~/.config/...`, `~/.zshrc`, etc.) by
`create_symlinks.sh`. **Add every new link to `lib/symlink-manifest.sh`** --- it
is the single source of truth, consumed by both `create_symlinks.sh` (to create)
and `dotfiles doctor` (to verify). Repo filenames usually drop the leading dot
(`~/.zshrc` -> `zshrc`).

Agent settings files that their tool rewrites (`claude/settings.json`,
`antigravity/settings.json`) are merged into place instead
(`JSON_MERGE_MANIFEST`), so tool writes never dirty the repo. Put a key in the
enforced file (`<name>.json`) when the repo should own it on every machine, and
in the seed file (`<name>.seed.json`) when it's only a default that a tool
command such as `/model` may change; `lib/settings-merge.jq` has the rules.
**Never edit the live file to change a shared setting**: edit a layer and run
`create_symlinks.sh`.

Paths with no symlink targets, so absent from the manifest: `bin/` (on `PATH`
via `zshenv`), `launchd/` (plists installed by hand, instructions in each
header), `lib/` (bash helpers), `tests/` (uv single-file tests, run directly:
`./tests/test_helix.py`), `backlog/` (Backlog.md tasks), `oxfmtrc.json` (shared
markdown/TOML format config).

Editor and multiplexer config: `helix/`, `ghostty/`, `rex/` (weddle runs the Rex
server from `systemd/user/rex.service` with the binary from `bin/rex-install`;
daysy's Rex.app connects to it). AI agents: `claude/`, `codex/`, `antigravity/`,
`agent-run/`.

### Private overlay

`private/` is a separate private repo (`benswift/dotfiles-private`), cloned
inside this checkout: gitignored here, re-included for rg/fd/helix by `.ignore`.
It holds mail, weddle's backups, private ssh hosts and anything else naming
institutional accounts or infrastructure, and has its own `AGENTS.md`.
`lib/private.sh` lists the hooks it plugs into. Nothing public may depend on it:
every hook is optional, so a machine without it still bootstraps.

**Changes under `private/` are invisible to this repo's git.** `git status` here
won't show them, and committing here won't include them. Commit and push them in
the overlay itself (`git -C private add <file>`, `git -C private commit`,
`git -C private push`). A change spanning both repos is two commits, and the
overlay's goes first when pushing: a public commit that expects a file the
overlay hasn't pushed breaks `dotfiles update` on every other machine.

`.github/workflows/security.yml` runs ruff, shellcheck, the portable test suite,
a lock audit (`scripts/audit-locks.py`) and gitleaks on every push. A tool the
tests call must be installed there too; the helix and todo-wiz suites stay local
(see the workflow).

## Python

- `bin/` tools are `uv run --script` with PEP 723 metadata and a committed
  `bin/<name>.lock`. After editing a dependency block, run
  `uv lock --script bin/<name>` and commit the lock.
- **Adding a `bin/` tool means adding it to `extend-include` in `ruff.toml`**,
  or it is silently never linted (a glob can't work --- it would drag the bash
  and swift scripts through the Python parser).

## Version control

Two GitLab instances are configured for `glab`:

- `gitlab.comp.anu.edu.au` --- teaching repos (comp2300, comp1720, lucy, etc.)
- `gitlab.anu.edu.au` --- jekyll-anu websites and other ANU projects

Set `GITLAB_HOST` when not inside a repo (e.g.
`GITLAB_HOST=gitlab.comp.anu.edu.au glab repo list`); inside a clone, `glab`
reads it from `origin`.

## Tool management (mise)

`mise/config.toml` (-> `~/.config/mise/config.toml`) sets global tool versions,
used when a project has no `mise.toml` of its own.

`~/.config/mise/config.local.toml` is machine-local, untracked, and auto-merged;
its `[env]` table is **the sole home for secrets** (`PUSHOVER_TOKEN`,
`PUSHOVER_USER_KEY`, `REPLICATE_API_TOKEN`). The private overlay's mail scripts
have their own backend (its `AGENTS.md`).

### Package installation hierarchy

When a tool can be installed several ways, prefer in order:

1. **mise** --- tools this repo's scripts need, plus development runtimes
   (python, node, go, rust)
2. **platform package manager** (brew on macOS, apt/dnf on Linux) --- system
   utilities (curl, git, jq) and tools needing OS integration
3. **language package managers** (`uv tool`, `bun add -g`, `cargo install`) ---
   only when the tool's docs say so, or mise doesn't support it

Avoid `cargo install` for anything available via mise or brew --- it compiles
from source. Pick one method per tool and stick to it, so `dotfiles doctor` can
verify it.

## Editor (helix)

Upstream has tagged no release since 25.07.1, so `bin/helix-build` compiles
helix from the commit in `helix/pinned-rev` and installs `hx` into
`~/.local/bin`. **Don't put helix back in mise** --- `mise activate` puts its
bin dir ahead of `~/.local/bin`, so a mise-managed copy silently shadows the
source build.

The script's header covers the mechanism and its usage (`--update` moves the
pin; commit `helix/pinned-rev` so the other machines follow). **Never run
`hx --grammar build` by hand**: it writes grammars that shadow the build's own.
`tests/test_helix.py`, run after every build, guards the setup.

## Formatting

`bin/claude-format` is a `Write|Edit` hook running the same per-type formatters
Helix runs on save (dispatch table in the script; mirrors
`helix/languages.toml`). Keep the two in sync.

## Utilities (`bin/`)

Each script's header says what it does and how; read it before using or changing
one.

The `wiz-due` units (`systemd/user/wiz-due.service`, its launchd twin) run every
registered vault's due workflows on this host, then `wiz sync -all`; anu's are
in the vault's `workflows/`, among them `health` (the job health check,
`bin/pkb-health`). `unit-oncall` and `launchd-run` file failures as anu tasks.

yazi's plugin dir is owned by `ya pkg`, which is why `yazi/` configs are
symlinked file-by-file rather than as a directory.

## AI coding agents

`AGENTS.md` is the project instructions file, read natively by Claude Code,
Codex and Antigravity CLI (`agy`); Claude loads it beside any `CLAUDE.md`
(`pluginConfigs` in `claude/settings.json`). For repos that still have only a
`CLAUDE.md`, Codex falls back to it (below) and `agy`'s `SessionStart` hook
(`antigravity/hooks.json`, running `bin/agy-project-context`) injects it.
Tracked config: `claude/CLAUDE.md` (global instructions, symlinked to
`~/.claude/CLAUDE.md`, `~/.codex/AGENTS.md` and `~/.gemini/AGENTS.md`),
`claude/settings.json`, `codex/hooks.json`, `antigravity/settings.json` and
`antigravity/hooks.json`.

### Personal skills

Skills live **only** in the private `benswift/claude-plugin-personal` repo.
Edit, commit and push in the working checkout at
`~/projects/claude-plugin-personal` (pull first), then run `dotfiles update` so
sessions load the change. **Never edit `~/.claude/plugins/marketplaces/ben/`**:
it is Claude Code's read-only copy, and every `dotfiles update` deletes and
re-clones it. Skills appear as `ben:<name>`.

`bin/sync-agent-config` registers marketplaces, installs enabled plugins, and
symlinks each skill into `~/.agents/skills` so Codex sees them too, and links
`agy`'s global skills directory (`~/.gemini/config/skills`) to it.

### Headless agent dispatcher

`bin/agent-run` runs unattended agent jobs; profiles in
`agent-run/profiles.toml` select the runner and auth route, the caller supplies
prompt, model and permissions. Subscription profiles are `*-sub`, API-billed
ones `*-api` (`agent-run/profiles.toml`'s comments say what each clears).
`openrouter` reaches third-party endpoints through Claude Code's
Anthropic-compatible surface; put its key in the untracked mise env.

A caller that passes no `--profile` (and no `AGENT_PROFILE`) gets the registry's
`default`, which is the one line to change when the subscription provider
changes. Options are namespaced per runner (`--claude-*`, `--codex-*`) and a
mismatched one is refused, not dropped. `--bypass-permissions` is the deliberate
exception --- runner-agnostic, so an unattended caller need not know its runner.

### Session log analytics

`bin/ship-claude-logs` sends each host's Claude and Codex session logs to weddle
hourly (systemd timer there; on macOS a step of the push-to-weddle launchd job).
`bin/ingest-claude-logs` summarises them into `~/claude-logs/analytics.db` and
fails when a host goes quiet, so weddle's ingester is every other host's alarm.
`bin/claude-find-session` searches the indexed sessions' prompts. The headers
have the detail.

### Per-agent notes

- **Claude Code** --- `.claude/` (with dot) is machine-local and gitignored;
  this repo tracks only `.claude/settings.json`, which disables the `impeccable`
  plugin (its `PostToolUse` hook drops `.impeccable/hook.cache.json` into
  whatever it thinks the project root is, a package's `src/` included).
- **Codex** --- `~/.codex/config.toml` is entirely machine-local and
  deliberately untracked; set `project_doc_fallback_filenames = ["CLAUDE.md"]`
  there on each machine, for repos without an `AGENTS.md`. There is no shared
  Codex profile: the `oy` alias just selects full-access mode.
- **Antigravity CLI** --- `modelProvider` in `antigravity/settings.json` bills
  `GEMINI_API_KEY` (untracked mise env) pay-as-you-go instead of signing in. The
  `gy` alias just selects full-access mode.
