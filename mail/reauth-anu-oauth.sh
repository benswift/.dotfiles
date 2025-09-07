#!/bin/bash

# Re-authorize ANU Office365 OAuth token for mbsync
# Uses Thunderbird's client ID which doesn't require a client secret
#
# NOTE: This script is only needed for:
# - Initial OAuth setup
# - Recovery when token is corrupted/deleted from keychain
# - When Microsoft revokes the refresh token (rare)
# - Setting up on a new device
#
# During normal operation, mutt_oauth2.py in the PassCmd automatically
# handles token refresh when tokens expire.

# Change to script directory
cd "$(dirname "$0")"

echo "Re-authorizing ANU Office365 OAuth token..."

# Remove old stub file if it exists
rm -f anu_oauth2_keychain_stub

# Run the OAuth authorization flow
# The empty echo provides an empty client secret (Thunderbird doesn't need one)
echo "" | ./mutt_oauth2.py \
    --provider microsoft \
    --email u2548636@anu.edu.au \
    --client-id 9e5f94bc-e8a4-4e73-b8be-63364c29d753 \
    --authorize \
    --authflow devicecode \
    --decryption-pipe 'security find-generic-password -a u2548636@anu.edu.au -s mutt_oauth2_anu -w' \
    --encryption-pipe './keychain-store.sh u2548636@anu.edu.au mutt_oauth2_anu' \
    anu_oauth2_keychain_stub

echo "Authorization complete. Token saved to keychain."