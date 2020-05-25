########
# PATH #
########

# add scripts from dotfiles repo
export PATH="$HOME/.dotfiles/scripts":$PATH

# for homebrew
if [ -x "$(command -v brew)" ]; then
	export PATH="/usr/local/sbin:$PATH"
fi

##########
# python #
##########

# pyenv

if [ -x "$(command -v pyenv)" ]; then
	eval "$(pyenv init -)"
	eval "$(pyenv virtualenv-init -)"
fi

# poetry

export PATH="$HOME/.poetry/bin:$PATH"
export POETRY_VIRTUALENVS_PATH="$HOME/.pyenv/versions"

# pipx

export PATH="$HOME/.local/bin":$PATH

########
# ruby #
########

# set up rbenv

if [ -x "$(command -v rbenv)" ]; then
	eval "$(rbenv init -)"
fi

#############
# Extempore #
#############

export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"

##########
# MATLAB #
##########

export PATH="/Applications/MATLAB_R2020a.app/bin:$PATH"

######
# QT #
######

# otherwise things are super-small on hidpi
# export QT_SCALE_FACTOR=2

# Local Variables:
# mode: sh
# End:
