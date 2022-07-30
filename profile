########
# PATH #
########

# NOTE: if you're using zsh, this should probably be linked to ~/.zprofile
# (rather than ~/.zshenv) because otherwise /etc/zprofile will be loaded and
# break your carefully-curated path

# add scripts from dotfiles repo
export PATH="$HOME/.dotfiles/scripts:$PATH"

# for homebrew
#
# this used to be wrapped in an "do only if brew command found" guard, but then
# I changed to zsh and things broke, and I can't be arsed fixing it right now.
eval "$(/opt/homebrew/bin/brew shellenv)"

########
# asdf #
########

. $(brew --prefix asdf)/libexec/asdf.sh

. ~/.asdf/plugins/java/set-java-home.zsh

###########
# haskell #
###########

if [ -f "$HOME/.ghcup/env" ]; then
    . "$HOME/.ghcup/env"
fi

#############
# Extempore #
#############

export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"

# Local Variables:
# mode: sh
# End:
