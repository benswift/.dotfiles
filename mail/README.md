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

5. **Optional: Configure msmtp** for scripted sending:
   - Install with `brew install msmtp`
   - Use same OAuth token from keychain
   - See `msmtprc` for configuration

See `reauth-oauth.sh`, `keychain-store.sh`, and `mutt_oauth2.py` in this
directory for working examples.
