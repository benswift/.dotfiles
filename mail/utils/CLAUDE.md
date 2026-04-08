# mail-utils

A Python package for working with maildir folders and email.

## Quick reference

Commands are available on PATH via `uv tool install -e ~/.dotfiles/mail/utils`:

```bash
mail-compose --help
mail-dedupe --help
mail-analyze --help
```

## Commands

| Command            | Description                                           |
| ------------------ | ----------------------------------------------------- |
| `mail-copy-path`   | Copy message file path to clipboard (uses mu to find) |
| `mail-compose`     | Compose and send emails with Jinja2 templating        |
| `mail-dedupe`      | Deduplicate messages by Message-ID                    |
| `mail-analyze`     | Analyze maildir format and identify issues            |
| `student-db`       | Query the PhD student database (denormalises from nb) |
| `mutt-compose-lsp` | Language server for mutt/neomutt compose buffers      |

## Usage

Each command has detailed examples in its `--help` output. Run
`<command> --help` for full options, flags, and usage patterns.

## Library usage

```python
from mail_utils.accounts import Account, get_account_config
from mail_utils.compose import build_email, send_email
from mail_utils.email import get_message_id, read_email_from_stdin
from mail_utils.maildir import open_maildir, is_maildir
from mail_utils.mu import find_message_path
```

## Development

```bash
cd ~/.dotfiles/mail/utils
uv run --group dev pytest -v      # run tests
uv run --group dev pytest -x      # stop on first failure
```

## Neomutt integration

The following neomutt macros use this package:

- `,p` - copy file path to clipboard

## Compose LSP

The `mutt-compose-lsp` command provides a language server for editing
mutt/neomutt compose buffers. It provides completions for:

- **Email addresses** in To/Cc/Bcc headers (via `mu cfind`)
- **File paths** in Attach headers (via `fd`)
- **Greetings** in the message body --- type `hey`, `hi`, `hello`, `g'day`, or
  `dear` and trigger completion to insert "Hey {Name}," based on the To: header

The greeting name lookup first checks a hardcoded `NICKNAMES` dict in the LSP
module (`src/mail_utils/lsp/__init__.py`), then falls back to the first name
from the recipient's display name.

Configured in helix via `~/.config/helix/languages.toml` for the `mutt-compose`
language (files matching `neomutt-*`).
