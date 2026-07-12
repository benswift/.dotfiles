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

Works on macOS (Apple Silicon) and Linux. The setup is zsh-only and mise-first
for tool management, using Homebrew on macOS and native package managers on
Linux.

## AI coding agents

This repo provides shared configuration for multiple AI coding agents, with
`CLAUDE.md` as the common project-level instructions file.

### How each agent uses these files

- **Claude Code**: `claude/CLAUDE.md` is symlinked to `~/.claude/CLAUDE.md`.
- **Codex CLI**: the same global instructions are symlinked to
  `~/.codex/instructions.md`, while the `dotfiles` Codex profile tells Codex to read
  project `CLAUDE.md` files. The `codex` and `oy` aliases select that profile
  automatically; trusted-project and UI state remain local in
  `~/.codex/config.toml`.
- **Gemini CLI**: `gemini/settings.json` tells Gemini to read `CLAUDE.md` and
  `GEMINI.md` as context files.

The `create_symlinks.sh` script sets up all necessary symlinks and directories.

### Personal skills plugin (`ben`)

My personal Claude Code skills library (pkb, benswift-writer, phdconvenor,
etc.) lives in a separate **private** repo,
[`benswift/claude-plugin-personal`](https://github.com/benswift/claude-plugin-personal),
loaded as a Claude Code plugin. The setup:

- **Registered via `install.sh` / `dotfiles update`** using
  `claude plugin marketplace add benswift/claude-plugin-personal` and
  `claude plugin install --scope user ben@ben`, then enabled in
  `claude/settings.json` via `enabledPlugins: {"ben@ben": true}` --- portable
  across machines, no hardcoded paths.
- **Claude Code clones it** to `~/.claude/plugins/marketplaces/ben/` on first
  use. That directory is the single source of truth --- edit skills there,
  commit and push from there.
- **Codex reads the same personal skills** via per-skill symlinks inside
  `~/.agents/skills`, Codex's supported user-level skill directory. Existing
  independently installed skills in that directory are left alone.
- **Skills appear to Claude** as `ben:<skill-name>` (e.g. `ben:pkb`) and to
  Codex by their unnamespaced skill name.

To propagate changes across machines: push from the marketplace clone, then run
`dotfiles update` elsewhere. That refreshes Claude plugins and re-syncs Codex
skill links. Requires SSH auth to GitHub (the repo is private).

### Elixir projects tip

The [Usage Rules](https://hexdocs.pm/usage_rules/) package is excellent for
injecting project-specific rules into your `CLAUDE.md` file:

```bash
mix usage_rules.sync CLAUDE.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing
```

## Email configuration

All email-related configuration has been organized into the `mail/` directory.
This includes configurations for mbsync (IMAP sync), msmtp (SMTP), neomutt
(email client), and OAuth2 authentication scripts.

For detailed setup instructions, see [`mail/README.md`](mail/README.md).

# License

(c) 2012-2025 Ben Swift

MIT License
