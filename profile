########
# PATH #
########

# add scripts from dotfiles repo
export PATH="$HOME/.dotfiles/scripts":$PATH

##########
# python #
##########

# pyenv

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# poetry

export PATH="$HOME/.poetry/bin:$PATH"

# pipx

export PATH="$HOME/.local/bin":$PATH

########
# ruby #
########

# set up rbenv

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

#############
# Extempore #
#############

export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"

##########
# MATLAB #
##########

export PATH="/Applications/MATLAB_R2020a.app/bin:$PATH"

# Local Variables:
# mode: sh
# End:
