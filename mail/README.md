# Email configuration

This directory contains all email-related configuration files for mbsync (IMAP
sync), msmtp (SMTP), and neomutt (email client).

The rc files are host-independent: paths are `$HOME`-relative and every secret
goes through `bin/mail-secret` (macOS login keychain, or mode-0600 files under
`~/.local/state/mail-secret/` on Linux --- see the script header). What differs
per host is which tier of the setup runs there, below.

## Files

- `mbsyncrc` - IMAP configuration for syncing mail with mbsync
- `msmtprc` - SMTP configuration for sending mail
- `neomutt/` - neomutt email client configuration
- `mutt_oauth2.py` - OAuth2 authentication script (from mutt source), driven by
  `mail-secret oauth`
- `reauth-anu-oauth.sh` - (re)authorise the Office365 OAuth token

## Tiers

**Full (daysy).** mbsync pushes and pulls every account into `~/Maildir` on a
five-minute launchd timer (`launchd/com.benswift.mailsync.plist`), mu indexes
it, neomutt and `mail-compose` read and send. This is the only host that syncs:
a second mbsync client on the same mailboxes is a second party to the
cross-device archive race described in TASK-025.

**Read-only plus send (weddle).** `~/Maildir` there is a mirror of daysy's,
refreshed by `bin/backup` (an `rclone sync` from daysy, sync-state files
included). Nothing on weddle may write into it --- mbsync would start from
daysy's stale state and upload local-only files as new, and the next backup
deletes anything written locally anyway. What runs:

- `mu` (apt `maildir-utils`) indexes the mirror --- it writes only to
  `~/.cache/mu`, never to the maildir --- so `mu find`, `mu view`,
  `mail-copy-path` and the compose LSP's address completion all work, as fresh
  as the last backup run
- `msmtp` (apt) sends, using the same rc file. `mail-compose --send` works; its
  sent copy never touches the maildir (see `utils/CLAUDE.md`)
- no mbsync, no neomutt

Setup on such a host:

```sh
sudo apt install maildir-utils msmtp
mu init --maildir ~/Maildir --my-address u2548636@anu.edu.au \
  --my-address ben.swift@anu.edu.au \
  --my-address phdconvenor.cybernetics@anu.edu.au \
  --my-address benswift@fastmail.com --my-address ben@benswift.me
mu index
# Fastmail: a NEW app password for this host, scoped to IMAP+SMTP (IMAP for
# the sent-folder append), so it can be revoked without touching daysy
mail-secret set benswift@fastmail.com mbsync-fastmail
# ANU: a separate device-code authorisation, so this host's refresh token is
# independent of daysy's
reauth-anu-oauth.sh
```

## Markdown email composition

Neomutt is configured with a macro (press `m` in compose mode) to convert
markdown emails to multipart/alternative format with both plaintext and HTML
versions. Write your email in markdown, then press `m` before sending to
generate the HTML version.

## Editor integration (LSP)

The `mutt-compose-lsp` command (part of `mail/utils/`) provides editor
completions for compose buffers: email addresses via mu, file paths for
attachments, and greeting expansions (type `hey` + complete to get "Hey
{Name},"). Configured in helix for the `mutt-compose` language.

## Zed extension

If you're a Zed user, you might find
[this zed extension for syntax highlighting of muttrc files and mutt compose buffers](https://github.com/benswift/zed-mutt)
helpful.

## Office365 setup

Both Office365 accounts (`anu`, and the delegated `phdconvenor` mailbox)
authenticate as `u2548636@anu.edu.au` over XOAUTH2 with one shared token.

1. **mbsync with XOAUTH2 support** (full tier only):

   ```sh
   brew install benswift/tap/isync
   ```

   homebrew-core's isync is compiled without SASL, so it can't do OAuth2. The
   [`benswift/homebrew-tap`](https://github.com/benswift/homebrew-tap) build
   links a `cyrus-sasl` that bundles the
   [cyrus-sasl-xoauth2](https://github.com/moriyoshi/cyrus-sasl-xoauth2) plugin,
   so XOAUTH2 works out of the box --- no `SASL_PATH` and no self-compiled
   binary. `install.sh` runs this automatically on macOS. On Linux, Ubuntu's
   isync (1.4.x) predates the `TLSType` syntax this config uses: build 1.5 from
   source and install `libsasl2-modules-kdexoauth2`.

2. **Token management** is `mail-secret oauth anu`: it wraps `mutt_oauth2.py`
   with Thunderbird's client id (a public client ANU permits, no secret) and the
   secret store as both encryption and decryption pipe, refreshing the access
   token as needed. The rc files call it directly as `PassCmd` / `passwordeval`.
   Run `reauth-anu-oauth.sh` for the initial device-code authorisation
   (localhostauthcode doesn't work with Thunderbird's id), on a new machine, or
   after Microsoft revokes the refresh token.

3. **msmtp** has XOAUTH2 built in and needs no plugin; `brew install msmtp` or
   the distro package.
