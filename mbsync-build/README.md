# mbsync with SASL/OAuth2 support

This directory contains mbsync (isync) version 1.5.1 built from source with SASL support for OAuth2 authentication.

## Build details

- Version: isync 1.5.1 (latest stable)
- Features: +HAVE_LIBSSL +HAVE_LIBSASL +HAVE_LIBZ +USE_DB +HAVE_IPV6
- Dependencies: berkeley-db@5, openssl@3, cyrus-sasl

## Usage

The compiled binary is located at: `./src/mbsync`

To use this version instead of the Homebrew version:
```bash
# Use directly
./src/mbsync [options]

# Or create an alias
alias mbsync='/Users/ben/.dotfiles/mbsync-build/isync-1.5.1/src/mbsync'

# Or add to PATH
export PATH="/Users/ben/.dotfiles/mbsync-build/isync-1.5.1/src:$PATH"
```

## OAuth2 configuration

To use OAuth2 with Office 365, configure your `.mbsyncrc` with:
```
IMAPAccount office365
Host outlook.office365.com
User your-email@domain.com
AuthMechs XOAUTH2
PassCmd "your-oauth2-token-command"
SSLType IMAPS
```

The PassCmd should return a properly formatted OAuth2 token for authentication.