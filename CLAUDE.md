# Ben's dotfiles repo

This repo contains all of the (public) config files that I use on my machines.

**DO NOT EVER ADD RAW CREDENTIALS/PASSWORDS TO THIS REPO.**

## Philosophy

- modern, zsh-only (no bash configs)
- cross-platform: macOS (Apple Silicon) and Linux
- mise-first for tool version management
- native package managers: Homebrew on macOS only, use apt/dnf/etc on Linux (no
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

All these files are expected to be in specific locations (e.g. @~/.config). The
@./create_symlinks.sh script links all the files in this repo into their
expected locations. The full list of links lives in @lib/symlink-manifest.sh ---
the single source of truth consumed by both @./create_symlinks.sh (to create)
and `dotfiles doctor` (to verify), so add new links there. In many cases the
filename in this repo doesn't have the preceding dot, but the symlink source
does (e.g. `~/.zshrc` is linked to `zshrc`).

Apart from a few one-off config files, these config files and scripts are
grouped into the following categories.

Four paths hold no symlink targets and so appear nowhere in the manifest: @lib/
(bash helpers sourced by the scripts --- `log.sh` and the manifest itself),
@tests/ (uv single-file tests for the `bin/` scripts, run directly:
`./tests/test_svg_validate.py`), `backlog/` (Backlog.md task files), and
@oxfmtrc.json (the shared markdown/TOML format config, used by `prettify-md` and
@bin/oxfmt-helix).

## Shell

Shell config stuff is in the top-level. I mostly use zsh (on macOS), so I have
@zshrc and @zshenv files. The prompt is a plain `PS1` --- no starship or
equivalent.

## Version control (git)

This repo uses plain git. Use `gh` CLI for GitHub API operations (PRs, issues,
etc.).

For GitLab, use `glab` CLI. Two instances are configured:

- `gitlab.comp.anu.edu.au` --- teaching repos (comp2300, comp1720, lucy, etc.)
- `gitlab.anu.edu.au` --- jekyll-anu websites and other ANU projects

Set `GITLAB_HOST` to target a specific instance when not inside a repo (e.g.
`GITLAB_HOST=gitlab.comp.anu.edu.au glab repo list`). Inside a cloned repo,
`glab` picks the host from the `origin` remote automatically.

Git configuration files are in the top-level:

- @gitconfig - global git configuration
- @gitignore - global gitignore patterns

## Utilities

The @bin/ directory contains utility scripts for system tasks (backup scripts,
etc.). Key scripts:

- `dotfiles` --- main management command (doctor, update, edit)
- `mailsync` --- sync all email accounts
- `claude-zellij`, `codex-zellij`, `gemini-zellij` --- zellij wrappers for AI
  agents
- `zj-switch` --- `Alt s` session switcher (live sessions only, most-recently
  used first, annotated with each session's Claude agents and their state). See
  "Session switching" below
- `agenda` --- read/create ANU Exchange calendar events via EventKit
- `teams` --- read/send Teams DMs by driving the web client
- `pkb-agent` --- run scheduled "EA" tasks (headless claude over the notebook);
  task definitions live in `~/.nb/home/tasks/`, one systemd timer
  (`pkb-agent.timer`, weddle only) fires everything due
- `ts-cat`, `ts-grammars` --- tree-sitter syntax highlighting for yazi's preview
  pane (see "File manager (yazi)" below)

See the "Microsoft 365 (calendar and Teams)" section below for this tooling and
why it sidesteps Microsoft Graph.

## Tool management (mise)

Global tool versions are defined in @mise/config.toml. This file is symlinked to
`~/.config/mise/config.toml` and provides fallback versions for tools when not
in a project with its own `mise.toml`.

Machine-local config (per-host env vars) goes in
`~/.config/mise/config.local.toml` --- mise auto-merges it with the global
config, so any `[env]` entries get exported into every shell.

Secrets default to fnox: resolve them through `fnox.toml` entries with `op://`
references backed by 1Password. Low-stakes API tokens consumed by agent
subprocesses (e.g. `PUSHOVER_TOKEN`/`PUSHOVER_USER_KEY` for `notify-pushover`,
`REPLICATE_API_TOKEN` for `styled-image-gen`) may live plaintext in
`config.local.toml` instead --- the file is untracked, so nothing secret lands
in git --- but higher-stakes credentials (e.g. `ANU_PASSWORD`) belong behind
fnox/1Password. Either way, nothing secret goes in a tracked file.

### Package installation hierarchy

When a tool can be installed multiple ways, prefer this order:

1. **mise** --- for tools required by scripts and configs in this repo, plus
   development runtimes (python, node, go, rust). Ensures consistent versions
   across machines and makes dependencies explicit in @mise/config.toml.
2. **Platform package manager** (brew on macOS, apt/dnf on Linux) --- for system
   utilities (curl, git, jq) and tools needing OS integration. Fast, prebuilt
   binaries.
3. **Language package managers** (`uv tool`, `bun add -g`, `cargo install`) ---
   only when the tool's documentation explicitly recommends this method, or mise
   doesn't support the tool.

Avoid `cargo install` for tools available via mise or brew---it compiles from
source, which is slow and resource-intensive. Use it only when the tool's docs
say to, or for Rust-specific tooling in Rust projects.

Pick one installation method per tool and stick with it so `dotfiles doctor` can
verify the setup.

## Text editors

### Zed

My Zed config is in the @zed/ folder.

### Helix

My Helix config is in the @helix/ folder. This includes:

- @helix/config.toml - main configuration (theme, editor settings, keybindings)
- @helix/languages.toml - language server and formatter configuration

## Terminal multiplexer

### Zellij

My Zellij config is in the @zellij/ folder. This includes:

- @zellij/config.kdl - theme, plus a full `clear-defaults=true` keybind block
- @zellij/layouts/dev.kdl - dev layout (hx + claude-yolo + terminal)

`config.kdl` is a fixed point of `kdlfmt format -`, the same command
@bin/claude-format and Helix run on save. Keep it that way, or a one-line
keybind change lands as a 500-line reflow.

#### Session switching

`Alt s` runs @bin/zj-switch in a floating pane. It lists only **live** sessions,
most-recently-used first, so `Alt s` then Enter returns to the previous session
the way cmd-tab does. Zellij's built-in session manager --- which also creates
sessions and resurrects dead ones --- stays on `Ctrl o` then `w`.

Each row is annotated with the Claude Code agents in that session (`⏳` working,
`⚠` blocked, `✓` idle, `·` unknown). The two halves come from different places
on purpose:

- **how many agents**: the process tree. Each session's server runs as
  `zellij --server <sockdir>/<session>` and every agent chains up to one, so a
  single `ps` is authoritative about who's alive.
- **what they're doing**: state files under
  `$XDG_RUNTIME_DIR/claude-agent-state/<session>/<claude-session-id>`, written
  by @bin/claude-turn-tracker from Claude Code's `SessionStart`,
  `UserPromptSubmit`, `Stop`, `Notification` and `SessionEnd` hooks.

State is therefore **reported, never inferred** --- nothing parses a pane's
title or its screen. That matters because Claude Code writes an OSC title
carrying an animated spinner glyph, so any title-scraping scheme breaks the next
time its UI changes. Each state file records the agent's pid, and `zj-switch`
drops entries whose pid is gone, so an agent killed without firing `SessionEnd`
doesn't linger.

The MRU stack lives at `$XDG_STATE_HOME/zj-switch/mru` and is maintained solely
by `zj-switch`: it pushes the current session on launch and the chosen one on
exit. A session first entered via `za`/`zs` shows up once you press `Alt s`
there, so the stack self-heals.

## File manager (yazi)

Yazi highlights its preview pane with syntect, which reads Sublime Text
`.sublime-syntax` grammars baked into the binary --- there's no config key to
add more. That set has no Astro grammar and no notion of language injection, so
YAML frontmatter in a markdown file renders as body text. Bat has the same gaps,
because it's the same asset set.

So previews are routed through tree-sitter instead, which does follow
injections: @yazi/yazi.toml prepends a previewer that pipes every text mime
through `piper.yazi` into @bin/ts-cat.

- @bin/ts-cat --- highlights a file to stdout with `tree-sitter highlight`,
  falling back to bat and then plain `cat` when no grammar matches. Two
  constraints come from piper and are load-bearing: anything written to stderr
  _replaces_ the preview (so all stderr is discarded, and the no-grammar case is
  detected by empty stdout), and piper kills the process once the pane is full.
  Also useful standalone as a `cat` that understands injections.
- @bin/ts-grammars --- clones the ~20 grammars into
  `~/.local/share/tree-sitter/grammars` (build artefacts, so not tracked here).
  Run by `install.sh` and `dotfiles update`; `--warm` pre-compiles them.
- @tree-sitter/config.json --- names the grammar directory and carries the
  catppuccin-mocha theme. The tree-sitter CLI derives its recognised capture
  names from the theme's keys, so a capture missing from that file renders
  unstyled.
- @tree-sitter/patches/ --- overlaid onto each checkout after every fetch. Four
  grammars need it: kdl, scss and astro predate the CLI's `tree-sitter.json`
  manifest, and typst ships an empty one with its queries nested a level down.
  The astro patch also adds an injections rule so a plain `<style>` block
  highlights as CSS (upstream only handles `lang="scss"`).

The plugin itself is pinned in @yazi/package.toml and installed with
`ya pkg install`. Note `~/.config/yazi/plugins` is owned by `ya pkg`, which is
why the yazi configs are symlinked file-by-file rather than as a directory.

Adding a language means appending its repo to the `GRAMMARS` array in
@bin/ts-grammars. If the extension doesn't match the grammar's declared
`file-types` (as with `.mdx`), add a scope override to the `case` in
@bin/ts-cat.

## AI coding agents

All three AI coding agents (Claude Code, Codex CLI, Gemini CLI) are configured
to read `CLAUDE.md` as the project-level instructions file. This means a single
file works across all tools with no symlinks needed. Global instructions and
base per-tool configuration are tracked here and symlinked into place.

### Claude Code

Three directories are involved --- note the differences:

- `claude/` (no dot) --- tracked config source in this public repo. It holds
  only `CLAUDE.md` (global instructions) and `settings.json`, both symlinked
  into `~/.claude/`. Deliberately **no** skills live here: personal skills are
  hosted exclusively in the `ben` plugin (below), so there's no `claude/skills/`
  directory and no `~/.claude/skills` symlink pointing back into dotfiles.
- The `ben` Claude Code plugin (personal skills library) lives in the
  **private** `benswift/claude-plugin-personal` repo. `install.sh` and
  `dotfiles update` run @bin/sync-agent-config, which registers the marketplace
  and installs enabled plugins via the `claude` CLI; @claude/settings.json then
  enables them via `enabledPlugins`. Claude Code maintains its own clone at
  `~/.claude/plugins/marketplaces/ben/`. That clone is the **single source of
  truth** --- edit skills there, commit and push from there. **Push immediately
  after committing**: `claude plugin marketplace update` (run by
  `sync-agent-config --update-claude`, i.e. every `dotfiles update`) deletes and
  re-clones the marketplace from GitHub, discarding local-only commits. If that
  happens, the tree at the last-installed commit survives as a snapshot under
  `~/.claude/plugins/cache/ben/ben/<sha>/`. Skills appear to the model as
  `ben:<skill-name>` (e.g. `ben:pkb`). The same bootstrap pattern handles the
  `impeccable` and `agent-browser` plugins.
- `.claude/` (with dot) --- project-local working directory auto-created by
  Claude Code. Contents are gitignored by default (`.claude/*` globally), but
  individual repos can opt-in to tracking specific subdirectories via a local
  `!.claude/<path>/` rule (e.g. project-local skills). The directory typically
  contains machine-specific state like `settings.local.json`, plans, and session
  data.

The @claude/ folder includes:

- @claude/CLAUDE.md - global agent instructions (symlinked to both
  `~/.claude/CLAUDE.md` and `~/.codex/instructions.md`)
- @claude/settings.json - Claude Code settings
- @codex/config.toml - portable Codex defaults
- @gemini/settings.json - portable Gemini context settings

Every personal skill lives in the ben plugin --- that is the single, exclusive
home, with no second copy tracked in this dotfiles repo. Each skill is a
directory at `~/.claude/plugins/marketplaces/ben/skills/<name>/SKILL.md` inside
Claude Code's marketplace clone, and is namespaced as `ben:<name>` when the
model loads it through the plugin mechanism. To add or edit a skill, work in
that clone (`~/.claude/plugins/marketplaces/ben/`), then commit and push from
there. No symlink wiring in @create_symlinks.sh is involved --- the marketplace
clone covers everything.

### Claude Code session logs (analytics)

Every host ships `~/.claude/projects` to `weddle:claude-logs/<host>/` via
@bin/ship-claude-logs --- a 15-minute systemd timer on weddle, a launchd agent
on macOS hosts (@launchd/, install instructions in each plist header), with a
metered-network guard so laptops never ship over a phone hotspot. On weddle,
@bin/ingest-claude-logs (hourly timer) rebuilds `~/claude-logs/analytics.db`
(one `sessions` row per session file, keyed by uuid + host + project dir).
Purpose: cross-machine introspection of Claude Code usage --- e.g. a recurring
agent proposing patterns worth reifying into skills.

### Codex CLI

Codex CLI uses `~/.codex/instructions.md` for global instructions (symlinked to
@claude/CLAUDE.md). Project-level instructions are read from `CLAUDE.md` via its
`project_doc_fallback_filenames` setting. Codex doesn't understand Claude Code's
plugin mechanism, but it reads the raw skill directories fine.

Personal skills are **not** currently synced into `~/.codex/skills`. The
`sync_codex_skills` function in @bin/sync-agent-config still does the work ---
it keeps `~/.codex/skills` as a Codex-owned directory, symlinks each personal
skill from `~/.claude/plugins/marketplaces/ben/skills/` into it, and leaves
Codex's generated `.system/` skills alone --- but the call is commented out at
the bottom of the script because Codex is unused at the moment. Whatever sits in
`~/.codex/skills` today is a stale snapshot from when it last ran. Restore the
call to bring the sync back.

`~/.codex/config.toml` is symlinked to @codex/config.toml, which holds only
portable defaults (model, reasoning effort, personality, project doc fallback).
Codex itself rewrites this file when it dismisses notices or records trusted
projects, so expect occasional uncommitted churn in the repo --- discard with
`git checkout codex/config.toml`, or run
`git update-index --skip-worktree codex/config.toml` per-machine if it gets
noisy. Machine-specific blocks (`[notice]`, `[tui.*]`, `[projects."..."]`)
should not be committed.

### Gemini CLI

Gemini CLI uses @gemini/settings.json to read `CLAUDE.md` as a context file (in
addition to the default `GEMINI.md`).

## Email

The email config lives in @mail/ and includes:

- @mail/neomutt/ - neomutt email client config
- @mail/msmtprc - SMTP configuration
- @mail/mbsyncrc - IMAP sync configuration
- email-processing scripts in @mail/utils/ (often in uv-powered single-file
  python scripts using the `mailbox` module)

See @mail/README.md for detailed setup instructions.

### Using neomutt with ht-mcp

To drive neomutt (or any TUI) interactively, use the `ht-mcp` MCP server. It's
installed via brew (`ht-mcp` binary) and registered globally in `~/.claude.json`
under `mcpServers.ht-mcp`. Tools appear under the `mcp__ht-mcp__` prefix:

1. `mcp__ht-mcp__ht_create_session` with `["bash"]` and `enableWebServer: true`
2. `mcp__ht-mcp__ht_execute_command` with `TERM=xterm-direct neomutt`
3. `mcp__ht-mcp__ht_send_keys` to send keystrokes (e.g. `["m"]`, `["Enter"]`,
   `["C-g"]`)
4. `mcp__ht-mcp__ht_take_snapshot` to view current terminal state. Note the
   snapshot is plain text and does NOT show which row has the cursor (the
   indicator is rendered via background colour). To probe cursor position, send
   a side-effect key like `<space>` (tag-entry) and check which row got the `*`
   flag, or rely on the resolve=yes cursor-advance behaviour
5. `mcp__ht-mcp__ht_close_session` when done

Useful for testing neomutt macros and workflows without GUI interaction.

## Microsoft 365 (calendar and Teams)

ANU locks down third-party Microsoft Graph app registrations and the device-code
flow, so there is no API path to ANU calendar or Teams data --- don't reach for
Graph here. It was tried and abandoned (no public client id works, and staff
can't self-register an app); the dead `sharepoint-dl` script that documented the
attempt has been removed. Two scripts in @bin/ instead go through channels ANU
does permit: the native macOS calendar stack, and the Teams web client signed in
as yourself.

### Calendar --- @bin/agenda

Native, registration-free calendar access via EventKit. The ANU Exchange account
is already synced into macOS Calendar.app --- it shows up as the calendar titled
"Calendar" under the "ANU Exchange" source --- and EventKit reads and writes it
directly, far faster than Calendar.app's AppleScript date-range queries. macOS
only; the first run prompts once for Calendar access (granted to the controlling
terminal).

- `agenda` --- upcoming events (default 7 days); takes `--days N`,
  `--cal "<name>"`, `--json`
- `agenda calendars` --- list calendars and their account source
- `agenda create --title "..." --start "2026-06-25 14:00" --end "..."` ---
  create an event (optional `--cal`, `--location`, `--notes`, `--all-day`).
  There is no `--attendee` option by design, so it only ever creates a personal
  time-block, never a meeting invite.

### Teams DMs --- @bin/teams

Reads and sends Teams chats by driving the Teams _web_ client with
`agent-browser`. Signing into teams.microsoft.com as yourself uses Microsoft's
own first-party client (which ANU permits), and the logged-in session lives in a
persistent Chrome profile at `~/.cache/agent-browser/teams-profile` on an
isolated `--session teams`. This is UI automation, so it's inherently more
brittle than the calendar tool --- expect occasional fix-ups when Microsoft
reshuffles the web client. Parsing leans on ARIA roles and labels rather than
positions to stay as durable as possible.

- `teams chats [N]` --- recent chats with previews and timestamps
- `teams read <name> [N]` --- a thread matched by a name substring (people or
  group chats)
- `teams send <name> <text>` --- post to a chat
- `teams login` --- interactive re-auth when the on-disk session expires (opens
  a window; tick "stay signed in")
- `teams status` --- login state

After the first interactive login the browser runs headless, reusing the on-disk
cookies. A cold start takes ~25--30s while Teams web loads; warm calls reuse the
running browser. Stop the background browser with
`agent-browser --session teams close`. Send clears the compose box (select-all +
delete) before typing, because Teams' contenteditable ignores an empty `fill`
and otherwise appends.
