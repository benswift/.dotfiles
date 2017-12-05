#!/usr/bin/env bash

set -euf -o pipefail

ln -sf $PWD/RProfile ~/.RProfile
ln -sf $PWD/gitconfig ~/.gitconfig
ln -sf $PWD/gitignore ~/.gitignore
ln -sf $PWD/mbsyncrc ~/.mbsyncrc
# ln -sf $PWD./profiles.clj ~/.lein/
ln -sf $PWD/scripts ~/bin
ln -sf $PWD/spacemacs-layers ~/.emacs.d/private/extempore
ln -sf $PWD/spacemacs ~/.spacemacs
ln -sf $PWD/ssh_config ~/.ssh/config

case "$OSTYPE" in
    darwin*) ln -sf $PWD/bash_profile.osx ~/.bash_profile && ln -sf $PWD/vscode-settings.json ~/Library/Application\ Support/Code/settings.json ;;
    linux*)  ln -sf $PWD/bash_profile.linux ~/.bash_profile ;;
    *)       echo "unknown OS: $OSTYPE" ;;
esac

