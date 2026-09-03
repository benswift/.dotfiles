#!/usr/bin/env bash
# (Re)authorise the ANU Office365 OAuth2 token: first setup, a new machine,
# or after Microsoft revokes the refresh token. Routine refresh is automatic
# inside `mail-secret oauth anu`, which mbsyncrc and msmtprc call directly.
exec "$(dirname "$0")/../bin/mail-secret" oauth anu --authorize --authflow devicecode
