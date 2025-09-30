# Ben's dotfiles repo

This repo contains all of the (public) config files that I use on my machines.

**DO NOT EVER ADD RAW CREDENTIALS/PASSWORDS TO THIS REPO.**

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
etc.).

## Zed (text editor)

My zed editor config is in the @zed/ folder.

## Claude Code

My Claude Code editor config is in the @claude/ folder (this includes agent
definitions).

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
