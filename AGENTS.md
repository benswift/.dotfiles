# Ben's dotfiles repo

This repo contains all of the (public) config files that I use on my machines.

**DO NOT EVER ADD RAW CREDENTIALS/PASSWORDS TO THIS REPO.**

## Philosophy

- modern, zsh-only (no bash configs)
- cross-platform: macOS (Apple Silicon and Intel) and Linux
- mise-first for tool version management
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

## Git

Git configuration files in the top-level:

- @gitconfig - global git configuration
- @gitignore - global gitignore patterns

## Utilities

The @bin/ directory contains utility scripts for system tasks (backup scripts,
etc.). Key scripts:

- `dotfiles` --- main management command (doctor, update, edit)
- `mailsync` --- sync all email accounts
- `claude-tmux`, `codex-tmux`, `gemini-tmux` --- tmux wrappers for AI agents

## Tool management (mise)

Global tool versions are defined in @mise/config.toml. This file is symlinked to
`~/.config/mise/config.toml` and provides fallback versions for tools when not
in a project with its own `mise.toml`.

## Text editors

### Zed

My Zed config is in the @zed/ folder.

### Helix

My Helix config is in the @helix/ folder. This includes:

- @helix/config.toml - main configuration (theme, editor settings, keybindings)
- @helix/languages.toml - language server and formatter configuration

## AI coding agents

### Claude Code

My Claude Code config is in the @claude/ folder. This includes:

- @claude/settings.json - Claude Code settings
- @claude/skills/ - skills that Claude loads dynamically based on task context

### Codex CLI

Codex CLI uses @~/.codex/instructions.md for global instructions (symlinked to
@GLOBAL-AGENTS.md). Project-level instructions use @codex.md (symlinked to
@AGENTS.md, gitignored). Skills are shared with Claude Code via a symlink from
@~/.codex/skills to the same @claude/skills/ directory.

### Gemini CLI

Gemini CLI is configured via @~/.gemini/settings.json to use `AGENTS.md` as a
context file (in addition to the default `GEMINI.md`). This allows it to reuse
the same project instructions as other agents without requiring additional
symlinks.

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
2. Set `EDITOR=vim` and run neomutt with `ht_execute_command`
3. Use `ht_send_keys` to send keystrokes (e.g., `["m"]` to compose mail)
4. Use `ht_take_snapshot` to view the current terminal state
5. Close the session with `ht_close_session` when done

This allows testing neomutt workflows (like the markdown conversion macro)
without requiring GUI interaction.
