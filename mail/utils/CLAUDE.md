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
| `mail-urls`        | Extract URLs from a piped message and pick one in fzf |
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

## Student database

`student-db` reads the single normalised database at
`~/.nb/home/data/student-db.json` --- every student Ben has a stake in, whether
as SOCY convenor or as a supervisor in another school. There is no second file;
`ben-phd-students.json` was merged away because the ten students it duplicated
were the only place the two records ever disagreed.

The CLI's `students` command **defaults to the convenor cohort**:
`--school SOCY` and active statuses only. It's the right default for batch
convenor email but it hides completed students and Ben's non-SOCY students, so
`--all` is the honest starting point for anything else. The library
(`StudentDB.students(...)`) applies no defaults --- every filter is off unless
asked for, and the convenor defaults live only in the CLI layer.

Two schema points that are easy to get wrong:

- `panel_ids` holds the **associate supervisors only** --- panel minus primary
  minus chair. The Chair of Panel goes in `panel_chair_id` (often, but not
  always, the primary supervisor), and the separate Confirmation Review Panel
  chair in `crp_chair_id`.
- `Person.name` is the display name and the only one that belongs in prose.
  `legal_name` is what ANU's systems hold, which is sometimes a name nobody uses
  (Sui Jackson is "Paul Jackson" in Dynamics) --- use it to search HORUS or
  Wattle, never to address someone.

`ben_role` on the denormalised output is derived from the ID references rather
than stored, so it cannot drift from the panel it describes.

## Development

```bash
cd ~/.dotfiles/mail/utils
uv run --group dev pytest -v       # run tests
uv run --group dev pytest -x       # stop on first failure
uv run --group dev ty check src/   # type check
ruff check src/ tests/             # lint (ruff comes from mise, not a dev dep)
```

All four are expected to be clean before a commit.

## Neomutt integration

The following neomutt macros use this package:

- `,p` - copy file path to clipboard
- `,b` - extract URLs; open in browser or compose a mailto (`mail-urls`)

`mail-urls` reads the raw RFC822 message on stdin (the macro clears
`pipe_decode` first so the full MIME structure is preserved), parses the HTML
with an actual parser to pull real `href`/`src`/anchor-text, dedupes, and hands
the list to fzf. Enter opens the selected web link(s) in the default browser,
Tab multi-selects, and Ctrl-Y copies instead. This replaced `urlscan`, whose
regex-over-decoded-text approach missed most links in HTML mail.

A selected `mailto:` link is composed in neomutt itself rather than handed to a
GUI mail client: `mail-urls` writes `push "<mail>ADDR<enter>"` to
`/tmp/neomutt-urls-commands`, which the `,b` macro sources after the pipe
returns (the same command-file pattern as the markdown-compose macro). The file
is always rewritten --- empty when there's nothing for neomutt to do --- so the
`source` is a harmless no-op otherwise. Only the address is carried across (any
`?subject=`/`?body=` in the mailto is dropped); the compose lands at the
`Subject:` prompt.

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
