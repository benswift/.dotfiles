# Ben's dotfiles repo

The (public) config files I use on my machines.

**DO NOT EVER ADD RAW CREDENTIALS/PASSWORDS TO THIS REPO.**

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

- `dotfiles doctor` --- check if everything is set up correctly
- `dotfiles update` --- pull latest and re-run symlinks
- `dotfiles edit` --- open dotfiles in editor
- `dotfiles cd` --- print the dotfiles directory (for `cd "$(dotfiles cd)"`)
- `./install.sh` --- bootstrap a new machine (also works via curl)
- `./create_symlinks.sh` --- create/update symlinks only

## Repo structure

Config files are symlinked into place (`~/.config/...`, `~/.zshrc`, etc.) by
@create_symlinks.sh. **Add every new link to @lib/symlink-manifest.sh** --- it
is the single source of truth, consumed by both `create_symlinks.sh` (to create)
and `dotfiles doctor` (to verify). Repo filenames usually drop the leading dot
(`~/.zshrc` -> `zshrc`).

Paths with no symlink targets, so absent from the manifest: @bin/ (on `PATH` via
@zshenv), @launchd/ (plists installed by hand, instructions in each header),
@lib/ (bash helpers), @tests/ (uv single-file tests, run directly:
`./tests/test_nb.py`), `backlog/` (Backlog.md tasks), @oxfmtrc.json (shared
markdown/TOML format config).

Editor and multiplexer config: @zed/, @helix/, @zellij/, @ghostty/. Email:
@mail/ (see @mail/README.md). AI agents: @claude/, @codex/, @gemini/, @matilda/,
@agent-run/.

## Python

- `bin/` tools are `uv run --script` with PEP 723 metadata and a committed
  `bin/<name>.lock`. After editing a dependency block, run
  `uv lock --script bin/<name>` and commit the lock.
- **Adding a `bin/` tool means adding it to `extend-include` in @ruff.toml**, or
  it is silently never linted (a glob can't work --- it would drag the bash and
  swift scripts through the Python parser).
- @mail/utils is a real package, installed editable by `install.sh`. It provides
  `mutt-compose-lsp`, which @helix/languages.toml configures as a language
  server, so a machine without it has a quietly broken editor.

## Version control

Plain git. Use `gh` for GitHub API operations, `glab` for GitLab. Two GitLab
instances are configured:

- `gitlab.comp.anu.edu.au` --- teaching repos (comp2300, comp1720, lucy, etc.)
- `gitlab.anu.edu.au` --- jekyll-anu websites and other ANU projects

Set `GITLAB_HOST` when not inside a repo (e.g.
`GITLAB_HOST=gitlab.comp.anu.edu.au glab repo list`); inside a clone, `glab`
reads it from `origin`.

## Tool management (mise)

@mise/config.toml (-> `~/.config/mise/config.toml`) sets global tool versions,
used when a project has no `mise.toml` of its own.

`~/.config/mise/config.local.toml` is machine-local, untracked, and auto-merged;
its `[env]` table is **the sole home for secrets** (`PUSHOVER_TOKEN`,
`PUSHOVER_USER_KEY`, `REPLICATE_API_TOKEN`, `ANU_PASSWORD`). The one 1Password
call left is @bin/vpn, falling back to `op read` when `ANU_PASSWORD` is unset.

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

## Formatting

@bin/claude-format is a `Write|Edit` hook running the same per-type formatters
Helix runs on save (dispatch table in the script; mirrors
@helix/languages.toml). Keep the two in sync.

**KDL files must stay fixed points of `kdlfmt format --kdl-version v1 -`.** The
`v1` is load-bearing: kdlfmt otherwise formats to whichever version parses, and
@zellij/layouts/dev.kdl is valid v2, which comes back with strings unquoted
(`command="hx"` -> `command=hx`). Zellij's parser is v1, where a bare word is
not a value, so the layout silently breaks.

## Utilities (@bin/)

Each script's header has the detail. Notable ones:

- `dotfiles` --- doctor, update, edit
- `mailsync` --- sync all email accounts
- `claude-zellij`, `codex-zellij`, `gemini-zellij` --- zellij wrappers
- `agent-run` --- headless agent dispatcher (see below)
- `zj-switch` --- `Alt s` session switcher, live sessions MRU-first, annotated
  with each session's running agents
- `claude-turn-tracker` --- Pushover notifications, plus zellij pane rename to
  `⚠ <reason>` while an agent is blocked (undone on the next prompt)
- `agenda` --- ANU Exchange calendar via EventKit (macOS only)
- `teams` --- Teams DMs via the web client
- `pkb-agent` --- scheduled "EA" tasks over the notebook; definitions live in
  `~/.nb/home/tasks/`, one systemd timer (weddle only) fires everything due
- `ai-tropes` --- screen a draft for AI-writing tells; taxonomy lives in the
  `ben:benswift-writer` skill's `ai-tropes.md`, cross-referenced by pattern id
- `ts-cat`, `lumis-parsers` --- tree-sitter highlighting for yazi previews and
  `nb show`, via lumis. Add a language to `PARSERS` in `lumis-parsers` if it's
  previewed often; add a `case` override in `ts-cat` if the extension doesn't
  map to a parser (as with `.mdx`)
- `pi-kiosk` --- flash an SD card booting a Raspberry Pi into a fullscreen
  Chromium kiosk, with wifi and Tailscale baked in (cloud-init, RPi OS Trixie+)

`nb` is a pinned `github:xwmx/nb` mise tool; @bin/nb is the launcher that makes
bare `nb` resolve the pin in non-interactive agent shells. @tests/test_nb.py
covers it against a temporary notebook, never `~/.nb`.

yazi's plugin dir is owned by `ya pkg`, which is why @yazi/ configs are
symlinked file-by-file rather than as a directory.

## AI coding agents

All of Claude Code, Codex CLI, Gemini CLI, Matilda Code and Grok Build read
`CLAUDE.md` as the project instructions file, so one file serves them all.
Tracked config: @claude/CLAUDE.md (global instructions, symlinked to both
`~/.claude/CLAUDE.md` and `~/.codex/AGENTS.md`), @claude/settings.json,
@codex/hooks.json, @gemini/settings.json, @matilda/system-defaults.json.

### Personal skills

Skills live **only** in the private `benswift/claude-plugin-personal` repo,
cloned by Claude Code to `~/.claude/plugins/marketplaces/ben/`. That clone is
the single source of truth: edit, commit and push from there, and **push
immediately** --- `claude plugin marketplace update` (run by every
`dotfiles update`) deletes and re-clones from GitHub, discarding local-only
commits. If that happens, the last-installed tree survives under
`~/.claude/plugins/cache/ben/ben/<sha>/`. Skills appear as `ben:<name>`.

@bin/sync-agent-config registers marketplaces, installs enabled plugins, and
symlinks each skill into `~/.agents/skills` so Codex and Matilda see them too.

### Headless agent dispatcher

@bin/agent-run runs unattended agent jobs; profiles in @agent-run/profiles.toml
select the runner and auth route, the caller supplies prompt, model and
permissions. Subscription-backed profiles (`claude-sub`, `codex-sub`,
`grok-sub`) clear API/gateway env vars first, so a scheduled job cannot silently
become pay-as-you-go. API-billed escape hatches are named `*-api`. `openrouter`
and `deepseek` reach third-party endpoints through Claude Code's
Anthropic-compatible surface; put their keys in the untracked mise env.

Options are namespaced per runner (`--claude-*`, `--codex-*`, `--grok-*`) and a
mismatched one is refused, not dropped. `--bypass-permissions` is the deliberate
exception --- runner-agnostic, so an unattended caller need not know its runner.

```sh
agent-run --profile claude-sub --model sonnet \
  --claude-dangerously-skip-permissions "/find-gigs"
agent-run --profile openrouter --model "provider/model:free" "prompt"
```

### Session log analytics

@bin/ship-claude-logs sends each host's `~/.claude/projects` and
`~/.codex/sessions` to weddle (systemd timer there, launchd on macOS).
@bin/ingest-claude-logs summarises both hourly into
`~/claude-logs/analytics.db`, one `sessions` row per session file, keyed on path
so ingest is incremental. Purpose: cross-machine introspection of agent usage.

The failure mode is a unit that succeeds and ships nothing, so
`ingest-claude-logs` exits non-zero when any host's newest session is older than
`AGENT_LOGS_STALE_DAYS` (default 7), with `OnFailure=unit-oncall@%n.service`.
weddle's ingester is every other host's alarm.

### Per-agent notes

- **Claude Code** --- `.claude/` (with dot) is machine-local and gitignored;
  this repo tracks only `.claude/settings.json`, which disables the `impeccable`
  plugin (its `PostToolUse` hook fires on every edit and drops
  `.impeccable/hook.cache.json` into whatever it thinks the project root is,
  including `mail/utils/src/`, from where it shipped in the built wheel).
- **Codex** --- `~/.codex/config.toml` is entirely machine-local and
  deliberately untracked; set `project_doc_fallback_filenames = ["CLAUDE.md"]`
  there on each machine. There is no shared Codex profile: the `oy` alias and
  @bin/codex-zellij just select full-access mode.
- **Grok Build** --- installed via mise's aqua backend so it's pinned rather
  than self-updating. `grok login --device-auth` works headless. Reads this
  repo's `CLAUDE.md` and Claude Code skills with no extra config; `grok inspect`
  prints what it found.
- **Matilda Code** --- npm-only, so its mise entry needs `allow_low_downloads`
  and `allow_builds`. `~/.matilda/settings.json` is Matilda's own (rewritten
  atomically by `auth login`, replacing a symlink with a real file), so
  @matilda/system-defaults.json holds the portable half instead, read in place
  via `MATILDA_CODE_SYSTEM_DEFAULTS_PATH`. It ships pre-stamped `"$version": 5`
  in Matilda's byte format, missing trailing newline and all --- an unstamped
  file is migrated in place on first run, dirtying the repo.
  - headless login is loopback-OAuth only, no device-code fallback.
    `MATILDA_AUTH_SKIP_BROWSER=1` (`AUTH`, not `OAUTH`) still prints an
    authorize URL pointing at a random `127.0.0.1:<port>` on that host. Open it
    on a machine with a browser, then either `curl` the failed
    `.../callback?code=...` on the headless host or `ssh -L` the port first ---
    the waiting process holds the PKCE verifier.
  - there's also an OpenAI-protocol endpoint at
    `https://matilda.maincode.com/api/v1/code` (model `matilda-code-1.0`, keyed
    by `MATILDA_API_KEY`) on the same subscription.

## Email

@mail/ holds neomutt config, @mail/msmtprc (SMTP), @mail/mbsyncrc (IMAP), and
email-processing scripts in @mail/utils/. Setup instructions: @mail/README.md.

To drive neomutt (or any TUI) interactively, use the `ht-mcp` MCP server
(`mcp__ht-mcp__*` tools): create a session with `["bash"]`, run
`TERM=xterm-direct neomutt`, then send keys and take snapshots. Snapshots are
plain text and do **not** show the cursor row (it's a background colour), so
probe position with a side-effect key like `<space>` (tag-entry) and see which
row gets the `*`.

## Microsoft 365 (calendar and Teams)

ANU locks down third-party Graph app registrations and the device-code flow, so
**there is no API path to ANU calendar or Teams data --- don't reach for
Graph.** Two scripts use channels ANU does permit:

- @bin/agenda --- EventKit against the ANU Exchange account already synced into
  macOS Calendar.app (the calendar titled "Calendar" under the "ANU Exchange"
  source). macOS only; first run prompts for Calendar access. Creates personal
  time-blocks only --- there is no `--attendee` option by design.
- @bin/teams --- drives the Teams web client via `agent-browser`, signed in as
  yourself in a persistent Chrome profile. Being UI automation it's brittle;
  expect fix-ups when Microsoft reshuffles the web client.
