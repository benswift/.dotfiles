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

### Personal skills plugin (`ben`)

My personal Claude Code skills library (pkb, benswift-writer, github-explorer,
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
- **Codex reads from the same directory** via a `~/.codex/skills` symlink (set
  up by `install.sh` and `dotfiles update` since the target doesn't exist until
  Claude Code has cloned the marketplace).
- **Skills appear to the model** as `ben:<skill-name>` (e.g.
  `ben:github-explorer`).

To propagate changes across machines: push from the marketplace clone, then run
`dotfiles update` elsewhere (which runs
`claude plugin update --scope user ben@ben`). Requires SSH auth to GitHub (the
repo is private).

### Elixir projects tip

The [Usage Rules](https://hexdocs.pm/usage_rules/) package is excellent for
injecting project-specific rules into your `AGENTS.md` file:

```bash
mix usage_rules.sync AGENTS.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing
```

## nb capture (iOS Shortcut)

The `bin/` directory contains an iOS Shortcut for quick todo capture to
[nb](https://xwmx.github.io/nb/), built with
[Cherri](https://github.com/electrikmilk/cherri). It supports text, voice, and
skip modes. Voice input is transcribed on weddle using Whisper large-v3 on GPU.

- `bin/nb-capture-todo.cherri` --- Shortcut source (compile with `cherri`)
- `bin/nb-capture-todo.py` --- server-side script that creates the todo
- `bin/nb-capture-server.py` --- HTTP server for receiving voice audio uploads
- `bin/nb-capture-server.service` --- systemd unit for the HTTP server

### weddle setup

The text/skip flows use SSH directly. The voice flow POSTs audio to an HTTP
server on weddle (the Shortcuts SSH action truncates large stdin payloads).

```bash
# install the systemd service
sudo cp ~/.dotfiles/bin/nb-capture-server.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now nb-capture-server
```

The Shortcuts-generated SSH key (separate from Secure ShellFish) must also be
added to `~/.ssh/authorized_keys` on weddle.

## Email configuration

All email-related configuration has been organized into the `mail/` directory.
This includes configurations for mbsync (IMAP sync), msmtp (SMTP), neomutt
(email client), and OAuth2 authentication scripts.

For detailed setup instructions, see [`mail/README.md`](mail/README.md).

# License

(c) 2012-2025 Ben Swift

MIT License
