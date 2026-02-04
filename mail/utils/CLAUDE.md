# mail-utils

A Python package for working with maildir folders and email.

## Quick reference

Get help for any command:

```bash
uv run --directory ~/.dotfiles/mail/utils mail-copy-id --help
uv run --directory ~/.dotfiles/mail/utils mail-compose --help
uv run --directory ~/.dotfiles/mail/utils mail-dedupe --help
uv run --directory ~/.dotfiles/mail/utils mail-analyze --help
uv run --directory ~/.dotfiles/mail/utils mail-fix-timestamps --help
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
| `mail-fix-timestamps` | Fix maildir timestamps and convert to mbsync format |

## Common usage

### Composing emails

```bash
# Interactive compose (opens neomutt)
mail-compose -f anu --to colleague@example.com -s "Hello" -b "Body"

# Send directly
mail-compose -f anu --to colleague@example.com -s "Hello" -b "Body" --send

# Batch send with template
mail-compose -f phdconvenor --data students.json \
    --to '{{email}}' \
    --subject 'Hello {{preferred_name}}' \
    --template body.md \
    --filter 'status == "active"' \
    --send
```

### Maildir operations

```bash
# Deduplicate a maildir
mail-dedupe ~/Maildir/anu/INBOX --dry-run
mail-dedupe ~/Maildir/anu/INBOX  # actually delete duplicates

# Analyze maildir format
mail-analyze single ~/Maildir/anu/INBOX
mail-analyze compare ~/Maildir/anu/INBOX ~/Maildir/personal/INBOX

# Fix timestamps
mail-fix-timestamps analyze ~/Maildir/imported
mail-fix-timestamps fix ~/Maildir/imported --dry-run
```

## Library usage

```python
from mail_utils import (
    Account, get_account_config,
    build_email, send_email,
    get_message_id, read_email_from_stdin,
    open_maildir, is_maildir,
    find_message_path,
)
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
