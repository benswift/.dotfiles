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
export PATH="/usr/local/bin:/usr/local/sbin:$PATH"

##########
# python #
##########

# for this python stuff to all to work nicely, you need
# - pyenv
# - pyenv-virtualenv
# - poetry
#
# however, you don't need to configure the pyenv-virtualenv stuff---just let
# poetry handle that

# pyenv

if [ -x "$(command -v pyenv)" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="${PYENV_ROOT}/bin:$PATH"
    eval "$(pyenv init --path)"
fi

# poetry

export PATH="$HOME/.poetry/bin:$PATH"
export POETRY_VIRTUALENVS_PATH="$HOME/.pyenv/versions"

alias pr='poetry run'

# pipx

export PATH="$HOME/.local/bin:$PATH"

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

if [ -x "$(command -v go)" ]; then
    export PATH="$PATH:$(go env GOPATH)/bin"
fi

###########
# haskell #
###########

# . "$HOME/.ghcup/env"

#############
# Extempore #
#############

export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"

# Local Variables:
# mode: sh
# End:
