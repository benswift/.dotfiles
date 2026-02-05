# mail-utils

A Python package for working with maildir folders and email.

## Installation

Install as an editable package:

```bash
uv tool install -e ~/.dotfiles/mail/utils
```

Or run commands directly via uv:

```bash
uv run --directory ~/.dotfiles/mail/utils mail-dedupe --help
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

## Library usage

```python
from mail_utils.email import get_message_id, read_email_from_stdin
from mail_utils.maildir import open_maildir, is_maildir

msg = read_email_from_stdin()
message_id = get_message_id(msg)

if is_maildir(path):
    mbox = open_maildir(path)
```

## Development

Run tests:

```bash
uv run --group dev pytest -v
```

## Maildir guidelines

### For Python scripts: Use the mailbox module

The `mailbox.Maildir` class provides proper handling of maildir semantics:

- Automatic management of cur/, new/, tmp/ subdirectories
- Proper handling of maildir info strings (flags after `:2,`)
- Safe message removal and manipulation
- Built-in locking and consistency guarantees

### Maildir structure

```
maildir/
├── cur/     # Read messages
├── new/     # Unread messages
├── tmp/     # Temporary files during delivery
```

### Filename format

Maildir filenames follow this pattern:

```
timestamp.unique.hostname,U=uid:2,FLAGS
```

Where FLAGS can include:

- `D` - Draft
- `F` - Flagged
- `P` - Passed (forwarded)
- `R` - Replied
- `S` - Seen (read)
- `T` - Trashed (marked for deletion)

### mbsync integration

When working with mbsync-managed maildirs:

- Never delete `.mbsyncstate*` files manually
- Mark messages with `T` flag instead of deleting directly
- Large `.mbsyncstate.journal` files indicate corruption
