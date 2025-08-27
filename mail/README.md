# Email configuration

This directory contains all email-related configuration files for mbsync (IMAP sync), msmtp (SMTP), and neomutt (email client).

## Files

- `mbsyncrc` - IMAP configuration for syncing mail with mbsync
- `msmtprc` - SMTP configuration for sending mail
- `neomutt/` - neomutt email client configuration
- `mutt_oauth2.py` - OAuth2 authentication script (from mutt source)
- `keychain-store.sh` - Helper script to store OAuth tokens in macOS Keychain
- `reauth-anu-oauth.sh` - Script to re-authenticate Office365 OAuth
- `anu_oauth2_keychain_stub` - Token stub file for Office365

## Office365 mbsync setup

To sync Office365 email with mbsync using OAuth2 authentication:

1. **Build mbsync with SASL support** (if not already available via package manager):
   - Source available at: https://github.com/isync-devel/isync
   - Configure with `--with-sasl` pointing to your SASL installation

2. **Install cyrus-sasl-xoauth2 plugin**:
   - Clone from: https://github.com/moriyoshi/cyrus-sasl-xoauth2
   - Build and install to your SASL plugin directory (e.g., `/opt/homebrew/Cellar/cyrus-sasl/*/lib/sasl2/`)

3. **OAuth2 token management**:
   - Use `mutt_oauth2.py` script (from mutt source) to obtain and refresh tokens
   - Configure with Thunderbird's client ID: `9e5f94bc-e8a4-4e73-b8be-63364c29d753`
   - Use devicecode flow for initial authentication (localhostauthcode doesn't work with Thunderbird)
   - Store tokens securely in macOS Keychain using `keychain-store.sh` wrapper
   - Run `./reauth-anu-oauth.sh` from the mail directory to re-authenticate

4. **Configure mbsync** (`mbsyncrc`):
   - Set `AuthMech XOAUTH2`
   - Use `PassCmd` with full paths to scripts
   - Example: `PassCmd "/Users/ben/.dotfiles/mail/mutt_oauth2.py --decryption-pipe 'security find-generic-password -a u2548636@anu.edu.au -s mutt_oauth2_anu -w' --encryption-pipe '/Users/ben/.dotfiles/mail/keychain-store.sh u2548636@anu.edu.au mutt_oauth2_anu' /Users/ben/.dotfiles/mail/anu_oauth2_keychain_stub"`

5. **Configure aerc** for Office365 SMTP:
   - Use `smtp+xoauth2://` protocol (not `smtps+oauthbearer://`)
   - Use actual username (e.g., `u2548636@anu.edu.au`) not display name
   - Server: `smtp.office365.com:587`
   - Token format: plain token (not SASL-encoded)

6. **Optional: Configure msmtp** for scripted sending:
   - Install with `brew install msmtp`
   - Use same OAuth token from keychain
   - See `msmtprc` for configuration

See `reauth-anu-oauth.sh`, `keychain-store.sh`, and `mutt_oauth2.py` in this directory for working examples.

## Troubleshooting

### mbsync "unexpected tag" error or hanging

If `mbsync` fails with an "unexpected tag" error or hangs, this usually indicates corrupted `.mbsyncstate` files. To fix:

```bash
# Remove all state files
rm -f ~/Maildir/anu/*/.mbsyncstate*
rm -f ~/Maildir/anu/*/.uidvalidity

# Check for large journal or lock files (especially in Archive folder)
ls -lah ~/Maildir/anu/*/.mbsyncstate*

# Test individual folders to identify problematic ones
for folder in INBOX Archive "Deleted Items" Drafts "Junk E-Mail" "Sent Items"; do
  echo "Testing $folder..."
  timeout 5 mbsync "anu:$folder"
done
```

This removes the state files and forces mbsync to rebuild them on the next sync. Note that this may cause mbsync to re-download message headers but won't duplicate emails. Large folders (like Archive with 40k+ messages) may take longer to sync initially after clearing state.