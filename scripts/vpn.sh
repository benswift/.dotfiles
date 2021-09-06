#!/usr/bin/env zsh

# pull ANU password out of encrypted authinfo file, pipe it to stdin
gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | \
    awk '/machine smtp.office365.com login u2548636@anu.edu.au/ {print $NF}' | \
    # start the VPN
    openconnect --user=u2548636 --protocol=gp --passwd-on-stdin https://staff-access.anu.edu.au
