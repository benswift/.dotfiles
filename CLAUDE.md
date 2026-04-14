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
expected locations. In many cases the filename in this repo doesn't have the
preceding dot, but the symlink source does (e.g. `~/.zshrc` is linked to
`zshrc`).

Apart from a few one-off config files, these config files and scripts are
grouped into the following categories.

## Shell

Shell config stuff is in the top-level. I mostly use zsh (on macOS), so I have
@zshrc and @zshenv files.

### Prompt (starship)

The shell prompt is configured via @starship.toml, symlinked to
`~/.config/starship.toml`. Uses gruvbox dark colours to match the helix theme.

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

## Tool management (mise)

Global tool versions are defined in @mise/config.toml. This file is symlinked to
`~/.config/mise/config.toml` and provides fallback versions for tools when not
in a project with its own `mise.toml`.

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
per-tool configuration (e.g. `~/.codex/config.toml`, `~/.gemini/settings.json`)
are managed per-machine, not tracked here.

### Claude Code

Three directories are involved --- note the differences:

- `claude/` (no dot) --- tracked config source in this public repo. `CLAUDE.md`
  (global instructions) and `settings.json` are symlinked into `~/.claude/`.
- The `ben` Claude Code plugin (personal skills library) lives in the
  **private** `benswift/claude-plugin-personal` repo. `install.sh` and
  `dotfiles update` register the marketplace and install the plugin via the
  `claude` CLI (`claude plugin marketplace add benswift/claude-plugin-personal`
  + `claude plugin install --scope user ben@ben`); @claude/settings.json
  then enables it via `enabledPlugins: {"ben@ben": true}`. Claude Code
  maintains its own clone at `~/.claude/plugins/marketplaces/ben/`. That
  clone is the **single source of truth** --- edit skills there, commit and
  push from there. `~/.codex/skills` is symlinked into the same directory so
  codex sees the same content. Skills appear to the model as
  `ben:<skill-name>` (e.g. `ben:github-explorer`). The same bootstrap
  pattern handles the `impeccable` and `agent-browser` plugins.
- `.claude/` (with dot) --- project-local working directory auto-created by
  Claude Code. Fully gitignored (both globally and in this repo). Contains
  machine-specific state like `settings.local.json`, plans, and session data.

The @claude/ folder includes:

- @claude/CLAUDE.md - global agent instructions (symlinked to both
  `~/.claude/CLAUDE.md` and `~/.codex/instructions.md`)
- @claude/settings.json - Claude Code settings

Skills live in the ben plugin at
`~/.claude/plugins/marketplaces/ben/skills/<name>/SKILL.md` (Claude Code's
marketplace clone) and are namespaced as `ben:<name>` when the model loads
them through Claude Code's plugin mechanism.

### Codex CLI

Codex CLI uses `~/.codex/instructions.md` for global instructions (symlinked to
@claude/CLAUDE.md). Project-level instructions are read from `CLAUDE.md` via its
`project_doc_fallback_filenames` setting. Codex doesn't understand Claude
Code's plugin mechanism, but it reads the raw skill directories fine via a
symlink from `~/.codex/skills` to `~/.claude/plugins/marketplaces/ben/skills/`
(created by the ben plugin bootstrap in @install.sh and @bin/dotfiles).

### Gemini CLI

Gemini CLI is configured per-machine to use `CLAUDE.md` as a context file (in
addition to the default `GEMINI.md`).

## Email

The email config lives in @mail/ and includes:

- @mail/neomutt/ - neomutt email client config
- @mail/msmtprc - SMTP configuration
- @mail/mbsyncrc - IMAP sync configuration
- email-processing scripts in @mail/utils/ (often in uv-powered single-file
  python scripts using the `mailbox` module)

See @mail/README.md for detailed setup instructions.

### Using neomutt with headless-terminal MCP

To drive neomutt interactively in this project, use the headless-terminal MCP
tools:

1. Create a session with `mcp__headless-terminal__ht_create_session` using
   `["bash"]` as the command and `enableWebServer: true`
2. Run neomutt with `ht_execute_command` using `TERM=xterm-direct neomutt`
3. Use `ht_send_keys` to send keystrokes (e.g., `["m"]` to compose mail)
4. Use `ht_take_snapshot` to view the current terminal state
5. Close the session with `ht_close_session` when done

This allows testing neomutt workflows (like the markdown conversion macro)
without requiring GUI interaction.
