# mail-utils

A Python package for working with maildir folders and email.

## Quick reference

Get help for any command:

```bash
uv run --directory ~/.dotfiles/mail/utils mail-copy-id --help
uv run --directory ~/.dotfiles/mail/utils mail-compose --help
uv run --directory ~/.dotfiles/mail/utils mail-dedupe --help
uv run --directory ~/.dotfiles/mail/utils mail-analyze --help
```

Or if installed as a tool (`uv tool install -e ~/.dotfiles/mail/utils`):

```bash
mail-compose --help
```

## Commands

| Command | Description |
|---------|-------------|
| `mail-copy-id` | Copy Message-ID to clipboard (reads email from stdin) |
| `mail-copy-path` | Copy message file path to clipboard (uses mu to find) |
| `mail-compose` | Compose and send emails with Jinja2 templating |
| `mail-dedupe` | Deduplicate messages by Message-ID |
| `mail-analyze` | Analyze maildir format and identify issues |
| `student-db` | Query the PhD student database (denormalises from nb) |
| `mutt-compose-lsp` | Language server for mutt/neomutt compose buffers |

## Common usage

### Composing emails

```bash
# Interactive compose (opens neomutt)
mail-compose -f anu --to colleague@example.com -s "Hello" -b "Body"

# Send directly
mail-compose -f anu --to colleague@example.com -s "Hello" -b "Body" --send

# Batch send with template (filter with jq before piping)
student-db students --status confirmed | \
    mail-compose -f phdconvenor --data - \
    --to '{{email}}' \
    --subject 'Hello {{preferred_name}}' \
    --template body.md \
    --send
```

### Student database queries

```bash
# List all students (denormalised with supervisor/panel details)
student-db students

# Filter by status
student-db students --status confirmed

# Pipe to jq for further filtering, then to mail-compose
student-db students --status pre-confirmation | \
  jq '[.[] | . as $s | {recipient: .supervisor, student: $s}]' | \
  mail-compose -f phdconvenor --data - --to '{{recipient.email}}' ...
```

### Maildir operations

```bash
# Deduplicate a maildir
mail-dedupe ~/Maildir/anu/INBOX --dry-run
mail-dedupe ~/Maildir/anu/INBOX  # actually delete duplicates

# Analyze maildir format
mail-analyze single ~/Maildir/anu/INBOX
mail-analyze compare ~/Maildir/anu/INBOX ~/Maildir/personal/INBOX
```

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

- `Ctrl+i` - copy Message-ID to clipboard
- `,p` - copy file path to clipboard

## Compose LSP

The `mutt-compose-lsp` command provides a language server for editing mutt/neomutt
compose buffers. It provides completions for:

- **Email addresses** in To/Cc/Bcc headers (via `mu cfind`)
- **File paths** in Attach headers (via `fd`)
- **Greetings** in the message body --- type `hey`, `hi`, `hello`, `g'day`, or
  `dear` and trigger completion to insert "Hey {Name}," based on the To: header

The greeting name lookup first checks a hardcoded `NICKNAMES` dict in the LSP
module (`src/mail_utils/lsp/__init__.py`), then falls back to the first name
from the recipient's display name.

Configured in helix via `~/.config/helix/languages.toml` for the `mutt-compose`
language (files matching `neomutt-*`).
