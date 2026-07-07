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
- `agenda` --- read/create ANU Exchange calendar events via EventKit
- `teams` --- read/send Teams DMs by driving the web client
- `pkb-agent` --- run scheduled "EA" tasks (headless claude over the notebook);
  task definitions live in `~/.nb/home/tasks/`, one systemd timer
  (`pkb-agent.timer`, weddle only) fires everything due

See the "Microsoft 365 (calendar and Teams)" section below for this tooling and
why it sidesteps Microsoft Graph.

## Tool management (mise)

Global tool versions are defined in @mise/config.toml. This file is symlinked to
`~/.config/mise/config.toml` and provides fallback versions for tools when not
in a project with its own `mise.toml`.

Machine-local config (per-host env vars, secrets not safe for git) goes in
`~/.config/mise/config.local.toml` --- mise auto-merges it with the global
config, so any `[env]` entries get exported into every shell. This is the sole
home for credentials (e.g. `PUSHOVER_TOKEN`/`PUSHOVER_USER_KEY` for
`notify-pushover`, `ANU_PASSWORD` for `vpn`, `REPLICATE_API_TOKEN` for
`styled-image-gen`); the file is untracked, so nothing secret lands in git.

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

- @zellij/config.kdl - main configuration (theme only, uses default keybindings)
- @zellij/layouts/dev.kdl - dev layout (hx + claude-yolo + terminal)

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
  truth** --- edit skills there, commit and push from there. Codex gets
  per-skill symlinks into that clone, while its generated `.system/` skills stay
  under `~/.codex/skills/.system`. Skills appear to the model as
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
there; @bin/sync-agent-config propagates it to Codex on the next
`dotfiles update`. No symlink wiring in @create_symlinks.sh is involved --- the
marketplace clone (for Claude Code) and the per-skill Codex symlinks (for Codex)
cover everything.

### Codex CLI

Codex CLI uses `~/.codex/instructions.md` for global instructions (symlinked to
@claude/CLAUDE.md). Project-level instructions are read from `CLAUDE.md` via its
`project_doc_fallback_filenames` setting. Codex doesn't understand Claude Code's
plugin mechanism, but it reads the raw skill directories fine.
@bin/sync-agent-config keeps `~/.codex/skills` as a Codex-owned directory and
symlinks each personal skill from `~/.claude/plugins/marketplaces/ben/skills/`
into it.

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
