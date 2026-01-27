# Ben's dotfiles

My dotfiles. They've gone through many ups and downs over the years, but at the
moment they're:

- text and terminal centric (hx, zellij, yazi)
- cross-platform (macOS and linux)
- designed to work just as well over ssh as run locally

Where I've taken anything from the web I've tried to give credit appropriate to
the licence. If I've missed giving you a shout out, then let me know and I'll
gladly add one in.

## Quick start

Bootstrap a new machine with:

```bash
curl -fsSL https://raw.githubusercontent.com/benswift/.dotfiles/main/install.sh | bash
```

This installs Homebrew and mise, clones the repo, creates symlinks, and installs
mise-managed tools.

## Installation (manual)

If you prefer to set things up manually:

```bash
git clone https://github.com/benswift/.dotfiles.git ~/.dotfiles
~/.dotfiles/create_symlinks.sh
```

## Verification

Check that everything is set up correctly:

```bash
dotfiles doctor
```

## Platform support

Works on macOS (Apple Silicon or Intel) and Linux with Homebrew. The setup is
zsh-only and mise-first for tool management.

## AI coding agents

This repo provides provider-agnostic configuration for multiple AI coding
agents, allowing them to share the same instruction files:

- **Global instructions**: `GLOBAL-AGENTS.md` contains instructions used across
  all projects
- **Project instructions**: `AGENTS.md` contains project-specific instructions
  (checked into each project's repo)

### How each agent uses these files

- **Claude Code**: Symlinks `GLOBAL-AGENTS.md` → `~/.claude/CLAUDE.md` and
  `AGENTS.md` → `CLAUDE.md` (gitignored)
- **Codex CLI**: Symlinks `GLOBAL-AGENTS.md` → `~/.codex/instructions.md` and
  `AGENTS.md` → `codex.md` (gitignored)
- **Gemini CLI**: Configured via `~/.gemini/settings.json` to read `AGENTS.md`
  directly (no symlinks needed)

The `create_symlinks.sh` script sets up all necessary symlinks and directories.

### Elixir projects tip

The [Usage Rules](https://hexdocs.pm/usage_rules/) package is excellent for
injecting project-specific rules into your `AGENTS.md` file:

```bash
mix usage_rules.sync AGENTS.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing
```

## Email configuration

All email-related configuration has been organized into the `mail/` directory.
This includes configurations for mbsync (IMAP sync), msmtp (SMTP), neomutt
(email client), and OAuth2 authentication scripts.

For detailed setup instructions, see [`mail/README.md`](mail/README.md).

# License

(c) 2012-2025 Ben Swift

MIT License
