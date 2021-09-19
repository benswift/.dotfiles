########
# PATH #
########

# add scripts from dotfiles repo
export PATH="$HOME/.dotfiles/scripts:$PATH"

# for homebrew
#
# this used to be wrapped in an "do only if brew command found" guard, but then
# I changed to zsh and things broke, and I can't be arsed fixing it right now.
export PATH="/usr/local/bin:/usr/local/sbin:$PATH"

##########
# python #
##########

# pyenv

if [ -x "$(command -v pyenv)" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
fi

# poetry

export PATH="$HOME/.poetry/bin:$PATH"
export POETRY_VIRTUALENVS_PATH="$HOME/.pyenv/versions"

alias pr='poetry run'

# pipx

export PATH="$HOME/.local/bin":$PATH

########
# ruby #
########

# set up rbenv

if [ -x "$(command -v rbenv)" ]; then
	eval "$(rbenv init -)"
fi

alias be='bundle exec'

######
# go #
######

export PATH=$PATH:$(go env GOPATH)/bin

###########
# haskell #
###########

. $HOME/.ghcup/env

#############
# Extempore #
#############

export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"

######
# QT #
######

# otherwise things are super-small on hidpi
# export QT_SCALE_FACTOR=2

# Local Variables:
# mode: sh
# End:
