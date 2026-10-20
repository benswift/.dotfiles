#!/bin/zsh
# Login shells only. Re-apply zshenv's PATH order after macOS's /etc/zprofile
# (path_helper) has reshuffled it --- see the header of zshenv. A no-op on
# Linux, where nothing reorders PATH between the two.
source "${ZDOTDIR:-$HOME}/.zshenv"
