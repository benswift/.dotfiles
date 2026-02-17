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

## Version control (jj)

This repo uses [jj](https://jj-vcs.github.io/jj/) (Jujutsu) as a colocated
repo (both `.jj/` and `.git/` exist). **Always use `jj` for version control
operations, never raw `git` commands.** The `gh` CLI is still fine for GitHub API
operations (PRs, issues, etc.).

Key jj commands:

- `jj st` --- show status
- `jj diff` --- show working copy changes
- `jj log` --- show history
- `jj describe -m "message"` --- set description on working copy change
- `jj new` --- start a new change (like creating a fresh commit to work on)
- `jj squash` --- squash working copy into parent
- `jj bookmark set <name>` --- set a bookmark (like a git branch pointer)
- `jj git push` --- push to remote
- `jj git fetch` --- fetch from remote
- `jj rebase -d <destination>` --- rebase current change

Git configuration files (still needed for the colocated git backend) are in the
top-level:

- @gitconfig - global git configuration
- @gitignore - global gitignore patterns

## Utilities

The @bin/ directory contains utility scripts for system tasks (backup scripts,
etc.). Key scripts:

- `dotfiles` --- main management command (doctor, update, edit)
- `mailsync` --- sync all email accounts
- `claude-zellij`, `codex-zellij`, `gemini-zellij` --- zellij wrappers for AI agents

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
3. **Language package managers** (`uv tool`, `pnpm add -g`, `cargo install`)
   --- only when the tool's documentation explicitly recommends this method, or
   mise doesn't support the tool.

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
file works across all tools with no symlinks needed.

### Claude Code

Two directories are involved --- note the difference:

- `claude/` (no dot) --- tracked config source, symlinked to `~/.claude/` on
  each machine. Contains `settings.json`, `skills/`, and a `.gitignore`
  whitelist.
- `.claude/` (with dot) --- project-local working directory auto-created by
  Claude Code. Fully gitignored (both globally and in this repo). Contains
  machine-specific state like `settings.local.json`, plans, and session data.

The @claude/ folder includes:

- @claude/settings.json - Claude Code settings
- @claude/skills/ - skills that Claude loads dynamically based on task context

### Codex CLI

Codex CLI uses @~/.codex/instructions.md for global instructions (symlinked to
@GLOBAL-AGENTS.md). Project-level instructions are read from `CLAUDE.md` via the
`project_doc_fallback_filenames` setting in @~/.codex/config.toml. Skills are
shared with Claude Code via a symlink from @~/.codex/skills to the same
@claude/skills/ directory.

### Gemini CLI

Gemini CLI is configured via @~/.gemini/settings.json to use `CLAUDE.md` as a
context file (in addition to the default `GEMINI.md`).

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
