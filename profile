########
# PATH #
########

# add scripts from dotfiles repo
export PATH="$HOME/.dotfiles/scripts":$PATH

##########
# python #
##########

# pyenv

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# poetry

export PATH="$HOME/.poetry/bin:$PATH"

# if this isn't set, probably a good idea to set it
# poetry config virtualenvs.in-project true

# pipx

export PATH="$HOME/.local/bin":$PATH

########
# ruby #
########

# set up rbenv

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
