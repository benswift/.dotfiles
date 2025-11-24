# Ben's dotfiles

My dotfiles. These days, [Zed](https://zed.dev) is the bright star around which
most of the rest of it revolves.

Where I've taken anything from the web I've tried to give credit appropriate to
the licence. If I've missed giving you a shout out, then let me know and I'll
gladly add one in.

## Installation

Run the included script to create symlinks for all config files:

```bash
./create_symlinks.sh
```

This will automatically link all dotfiles and Zed configuration files to their
expected locations.

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
