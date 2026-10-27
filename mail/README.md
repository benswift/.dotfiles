# Email configuration

This directory contains all email-related configuration files for mbsync (IMAP
sync), msmtp (SMTP), and neomutt (email client).

The rc files are host-independent: paths are `$HOME`-relative and every secret
goes through `bin/mail-secret` (macOS login keychain, or mode-0600 files under
`~/.local/state/mail-secret/` on Linux --- see the script header). What differs
per host is which tier of the setup runs there, below.

## Files

- `mbsyncrc` - IMAP configuration for syncing mail with mbsync
- `mbsyncrc-weddle`, `mbsyncrc-wiz` - pull-only variants for weddle and watson
- `msmtprc` - SMTP configuration for sending mail
- `neomutt/` - neomutt email client configuration
- `mutt_oauth2.py` - OAuth2 authentication script (from mutt source), driven by
  `mail-secret oauth`
- `reauth-anu-oauth.sh` - (re)authorise the Office365 OAuth token

## Tiers

**Full (daysy).** mbsync pushes and pulls every account into `~/Maildir` on a
five-minute launchd timer (`launchd/com.benswift.mailsync.plist`), mu indexes
it, neomutt and `mail-compose` read and send. This is the only host that pushes:
a second pushing client on the same mailboxes is a second party to the
cross-device archive race described in TASK-025.

**Pull-only (watson).** wiz's automation pulls just the lxconvenor mailbox with
`mbsyncrc-wiz`, whose header explains why it can't race daysy.

**Pull-only plus send (weddle).** `systemd/user/mail-pull.timer` pulls every
account every five minutes with `mbsyncrc-weddle` (pull-only, so it can't race
daysy either) into `/data/Maildir-imap`, which `~/Maildir` points at. The one
exception is `anu-archive`, which exists only on daysy and arrives via
`bin/backup`. Nothing else may write into the tree --- not neomutt, and never
the full `mbsyncrc`. What runs:

- mbsync 1.5.1, built from source into `~/.local` (below)
- `mu` (apt `maildir-utils`) indexes the mirror --- it writes only to
  `~/.cache/mu`, never to the maildir --- so `mu find`, `mu view`,
  `mail-copy-path` and the compose LSP's address completion all work.
  `mu-index.service` runs after each pull and after each anu-archive push
  (`mu-index.path`); enable both with
  `systemctl --user enable --now mail-pull.timer mu-index.path`
- `msmtp` (apt) sends, using the same rc file. `mail-compose --send` works; its
  sent copy never touches the maildir (see `utils/CLAUDE.md`)
- no neomutt

Setup on such a host:

```sh
sudo apt install maildir-utils msmtp libsasl2-modules-kdexoauth2 \
  libssl-dev libsasl2-dev zlib1g-dev libdb-dev
# isync 1.5.1 (Ubuntu's 1.4 predates TLSType); same tarball as daysy's formula
curl -fsSLO https://downloads.sourceforge.net/project/isync/isync/1.5.1/isync-1.5.1.tar.gz
echo "28cc90288036aa5b6f5307bfc7178a397799003b96f7fd6e4bd2478265bb22fa  isync-1.5.1.tar.gz" | sha256sum -c
tar xzf isync-1.5.1.tar.gz && cd isync-1.5.1
./configure --prefix="$HOME/.local" && make && make install
# msmtprc logs here; sending still works without the directory, but every
# send then prints a "cannot log" error
mkdir -p -m 700 ~/.local/state/msmtp
mu init --maildir ~/Maildir --my-address u2548636@anu.edu.au \
  --my-address ben.swift@anu.edu.au \
  --my-address phdconvenor.cybernetics@anu.edu.au \
  --my-address adir.hdr.cybernetics@anu.edu.au \
  --my-address lxconvenor.cybernetics@anu.edu.au \
  --my-address benswift@fastmail.com --my-address ben@benswift.me
mu index
# Fastmail: a NEW app password for this host, scoped to IMAP+SMTP (IMAP for
# the sent-folder append), so it can be revoked without touching daysy
mail-secret set benswift@fastmail.com mbsync-fastmail
# ANU: a separate device-code authorisation, so this host's refresh token is
# independent of daysy's
reauth-anu-oauth.sh
```

## Retention and the cold store

ANU's retention policy moves mail out of every Exchange mailbox two years after
it arrives, into that mailbox's Online Archive. IMAP can't see the Online
Archive, so to mbsync each move is a deletion, mirrored into `~/Maildir` on
every host. `~/Maildir/anu-archive/<account>/<folder>` is the local-only cold
store that keeps it: `mail-retain` (in `utils/`) copies INBOX, Archive and Sent
Items mail from all four Exchange accounts in once it is about 21 months old,
and `bin/backup` runs it on daysy before each hourly push of the store to
weddle. No mbsync channel touches the store, so nothing ever deletes from it. mu
indexes it with everything else; during the three-month overlap a message exists
in both places, which `mu find --skip-dups` hides.

To recover mail that has already aged out, move it in OWA from the Online
Archive back into the same folder of the primary mailbox. mailsync pulls it and
`mail-retain` copies it in; the policy moves it back out at its next run, which
mbsync sees as an ordinary delete. A backup that keeps the `<account>/<folder>`
layout (a restic snapshot of weddle's `/data/Maildir-imap`, say) works too, via
`mail-retain --source <restored-root>`. Either way it copies only what the store
lacks (keyed by Message-ID), so duplicates on the server don't reach the store.
Never copy recovered mail into an mbsync-managed folder by hand: unpaired
messages there upload as new (see below).

## Recovering a folder

A maildir and its `.mbsyncstate` are one unit: mbsync pairs each server UID with
a local file, and acts on whichever half is missing. So the obvious repairs are
the destructive ones on daysy (`Sync All`, `Expunge Both`):

- state deleted, messages kept: every message is unpaired on both sides, so each
  is copied across, doubling the folder on the server
- state kept, messages deleted (a botched move, an empty restore): each missing
  message reads as a local delete, and is deleted on the server
- messages restored from a backup into a folder whose state no longer lists
  them: they are unpaired, so they upload as new, duplicating them
- a maildir copied between hosts with its state: the receiving host inherits the
  sender's pairs, and any gap in the copy becomes a permanent hole

To rebuild a folder, stop mailsync, move the whole folder aside (messages,
`.mbsyncstate` and `.uidvalidity` together), let mbsync pull it fresh, then
reconcile anything local-only from the moved copy by hand. On weddle the same
move is harmless, since a pull-only channel can't touch the server. A folder
whose mbsync run failed mid-pull can record the failed messages as done; the fix
is the same move-aside and re-pull.

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

## Office365 setup

All four Office365 accounts (`anu`, the delegated `phdconvenor` mailbox and the
shared `adirh` and `lxconvenor` ones) authenticate as `u2548636@anu.edu.au` over
XOAUTH2 with one shared token.

1. **mbsync with XOAUTH2 support** (full and pull-only tiers):

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
   source and install `libsasl2-modules-kdexoauth2` (weddle's setup, above).

2. **Token management** is `mail-secret oauth anu`: it wraps `mutt_oauth2.py`
   with Thunderbird's client id (a public client ANU permits, no secret) and the
   secret store as both encryption and decryption pipe, refreshing the access
   token as needed. The rc files call it directly as `PassCmd` / `passwordeval`.
   Run `reauth-anu-oauth.sh` for the initial device-code authorisation
   (localhostauthcode doesn't work with Thunderbird's id), on a new machine, or
   after Microsoft revokes the refresh token.

3. **msmtp** has XOAUTH2 built in and needs no plugin; `brew install msmtp` or
   the distro package.
