#!/usr/bin/env zsh

# pull ANU password out of 1Password, pipe it to stdin
op item get "ANU Identity" --fields pw |\
    # start the VPN
    sudo openconnect --user=u2548636 --protocol=gp --passwd-on-stdin https://staff-access.anu.edu.au
