# Email reference

## Accounts

Three accounts configured, synced to local Maildir via mbsync:

- **personal**: Fastmail (~/Maildir/personal/) --- plain LOGIN auth
- **anu**: ANU Office365 (~/Maildir/anu/) --- XOAUTH2
- **phdconvenor**: delegated mailbox (~/Maildir/phdconvenor/) --- XOAUTH2 via
  anu credentials

Each has standard IMAP folders: INBOX, Archive, "Sent Items", Drafts,
Trash/"Deleted Items", "Junk Mail"/"Junk E-Mail".

For detailed setup, OAuth troubleshooting, and infrastructure docs, see
`~/.dotfiles/mail/README.md`.

## mu find --- searching emails

```bash
mu find from:sarah@example.com subject:timeline
mu find date:20240101..20240131 maildir:/anu/INBOX
mu find flag:attach size:1M..
mu find '(from:colleague OR to:colleague) AND subject:project date:1m..'
mu find body:"thesis submission" date:2024..
```

Output field codes (use with `--fields`):

- `d` = date, `f` = from, `s` = subject, `t` = to, `l` = file path, `c` = cc
- Default: `d,f,s`

```bash
mu find --fields "d,f,s" from:colleague date:1w..
mu find --fields "l" to:joy subject:babysit | head -1
```

## mu view --- reading email bodies

```bash
mu view ~/Maildir/anu/INBOX/cur/1234567890.12345_1.hostname
mu view --format plain message-file       # plain text (default)
mu view --format html message-file        # HTML body
mu view --summary-len 20 message-file     # first 20 lines only
mu view --decrypt message-file            # decrypt encrypted
```

Common workflow --- find then read:

```bash
mu find --fields "l" from:sarah date:1w.. | head -1 | xargs mu view
mu find --fields "l" maildir:/anu/"Sent Items" to:joy | head -1 | xargs mu view
```

## mu index --- maintaining the database

```bash
mu info                                   # check database status
mu index                                  # update after mbsync
mu index --rebuild                        # rebuild if corrupted
mu init --maildir ~/Maildir               # first-time init
```

Always check `mu info` before searching to ensure the database is current.

## mail-compose --- sending email

Prefer `mail-compose` for sending (available on PATH via uv tool):

```bash
# Direct send
mail-compose -f anu --to "Name <email>" -s "Subject" -b "Body" --send

# Dry run first
mail-compose -f anu --to "Name <email>" -s "Subject" -b "Body" --dry-run

# Batch send with template
student-db students --status confirmed | \
    mail-compose -f phdconvenor --data - --to '{{email}}' \
    --subject 'Hello {{preferred_name}}' --template body.md --send
```

See `~/.dotfiles/mail/utils/CLAUDE.md` for full CLI reference.

## neomutt --- interactive email

Use the headless-terminal MCP for interactive email only when needed (browsing
folders, complex compose workflows):

1. Create session: `mcp__headless-terminal__ht_create_session` with
   `{"command": ["bash"], "enableWebServer": true}`
2. Launch: `ht_execute_command` with `TERM=xterm-direct neomutt`
3. Navigate: `ht_send_keys` --- `/` search, `c` change folder, `m` compose
   (converts markdown to HTML), `Enter` select, `q` quit
4. View: `ht_take_snapshot`
5. Close: `ht_close_session`

Use neomutt only when interactive browsing or sending is needed. Default to
`mu find` + `mu view` for search and read operations.

## Email sync and sending

- **mbsync** syncs IMAP to local Maildir
- **msmtp** sends outgoing mail
- OAuth2 tokens for Office365 in macOS Keychain
- If OAuth expires: `~/.dotfiles/mail/reauth-anu-oauth.sh`
- For complex maildir operations (bulk processing, deduplication): write a
  Python script using `uv` and `mailbox.Maildir` --- see
  `~/.dotfiles/mail/utils/`
