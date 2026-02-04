# Email configuration

This directory contains all email-related configuration files for mbsync (IMAP
sync), msmtp (SMTP), and neomutt (email client).

## Files

- `mbsyncrc` - IMAP configuration for syncing mail with mbsync
- `msmtprc` - SMTP configuration for sending mail
- `neomutt/` - neomutt email client configuration
- `mutt_oauth2.py` - OAuth2 authentication script (from mutt source)
- `keychain-store.sh` - Helper script to store OAuth tokens in macOS Keychain
- `reauth-oauth.sh` - Script to re-authenticate Office365 OAuth
- `oauth2_keychain_stub` - Token stub file for Office365

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

## Office365 mbsync setup

To sync Office365 email with mbsync using OAuth2 authentication:

1. **Build mbsync with SASL support** (if not already available via package
   manager):

   - Source available at: https://github.com/isync-devel/isync
   - Configure with `--with-sasl` pointing to your SASL installation

2. **Install cyrus-sasl-xoauth2 plugin**:

   - Clone from: https://github.com/moriyoshi/cyrus-sasl-xoauth2
   - Build and install to your SASL plugin directory (e.g.,
     `/opt/homebrew/Cellar/cyrus-sasl/*/lib/sasl2/`)

3. **OAuth2 token management**:

   - Use `mutt_oauth2.py` script (from mutt source) to obtain and refresh tokens
   - Configure with Thunderbird's client ID:
     `9e5f94bc-e8a4-4e73-b8be-63364c29d753`
   - Use devicecode flow for initial authentication (localhostauthcode doesn't
     work with Thunderbird)
   - Store tokens securely in macOS Keychain using `keychain-store.sh` wrapper
   - Run `./reauth-oauth.sh` from the mail directory to re-authenticate

4. **Configure mbsync** (`mbsyncrc`):

   - Set `AuthMech XOAUTH2`
   - Use `PassCmd` with full paths to scripts
   - Example:
     `PassCmd "/path/to/mutt_oauth2.py --decryption-pipe 'security find-generic-password -a user@example.com -s mutt_oauth2_account -w' --encryption-pipe '/path/to/keychain-store.sh user@example.com mutt_oauth2_account' /path/to/oauth2_keychain_stub"`

5. **Configure msmtp** for sending:
   - Install with `brew install msmtp`
   - Use same OAuth token from keychain
   - See `msmtprc` for configuration

See `reauth-oauth.sh`, `keychain-store.sh`, and `mutt_oauth2.py` in this
directory for working examples.

## msmtpq queue setup

The email setup uses msmtpq to queue outgoing mail when offline and
automatically flush the queue when the network is available.

### Configuration

1. **Queue directory and config**:

   - Queue directory: `~/.msmtp.queue` (created with 0700 permissions)
   - Config file: `~/.msmtpqrc` defines queue and log locations
   - Queue log: `~/.msmtp.queue.log`

2. **Neomutt integration**:

   - All account configs use `msmtpq` instead of `msmtp` directly
   - Mail is queued automatically when offline
   - Sent immediately when online

3. **Automatic queue flushing**:

   - Managed by launchd agent:
     `~/Library/LaunchAgents/me.benswift.msmtpq-flush.plist`
   - Event-driven: flushes immediately on network state changes
   - Safety net: also flushes every 30 minutes as fallback
   - Throttled: 10-second minimum between flush attempts to prevent rapid-fire
     on network flapping

4. **Manual queue management**:
   ```bash
   msmtp-queue -d    # display queue contents
   msmtp-queue -r    # flush/run queue manually
   msmtp-queue -p    # purge specific mail(s)
   msmtp-queue -a    # purge all mail in queue
   ```

The launchd agent provides a pragmatic balance: fast response to network changes
with a periodic safety net for edge cases, without excessive polling.
