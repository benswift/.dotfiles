# Ben's dotfiles

My dotfiles. These days, [Zed](https://zed.dev) is the bright star around which
most of the rest of it revolves.

Where I've taken anything from the web I've tried to give credit appropriate to
the licence. If I've missed giving you a shout out, then let me know and I'll
gladly add one in.

## Installation

Run the included script to create symlinks for all config files:

```bash
./create_symlinks.sh
```

This will automatically link all dotfiles and Zed configuration files to their
expected locations.

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
   - Run `./reauth-anu-oauth.sh` from the dotfiles directory to re-authenticate

4. **Configure mbsync** (`.mbsyncrc`):
   - Set `AuthMech XOAUTH2`
   - Use `PassCmd` with full paths to scripts
   - Example: `PassCmd "/Users/ben/.dotfiles/mutt_oauth2.py --decryption-pipe 'security find-generic-password -a u2548636@anu.edu.au -s mutt_oauth2_anu -w' --encryption-pipe '/Users/ben/.dotfiles/keychain-store.sh u2548636@anu.edu.au mutt_oauth2_anu' /Users/ben/.dotfiles/anu_oauth2_keychain_stub"`

5. **Configure aerc** for Office365 SMTP:
   - Use `smtp+xoauth2://` protocol (not `smtps+oauthbearer://`)
   - Use actual username (e.g., `u2548636@anu.edu.au`) not display name
   - Server: `smtp.office365.com:587`
   - Token format: plain token (not SASL-encoded)

6. **Optional: Configure msmtp** for scripted sending:
   - Install with `brew install msmtp`
   - Use same OAuth token from keychain
   - See `.msmtprc` for configuration

See `reauth-anu-oauth.sh`, `keychain-store.sh`, and `mutt_oauth2.py` in this repo for working examples.

# License

(c) 2012-2025 Ben Swift

MIT License
