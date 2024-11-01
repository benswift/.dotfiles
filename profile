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
# ruby #
########

# if it breaks too many things I'll disable it, but I'm a believer so far...
# https://benswift.me/blog/2023/02/02/jekyll-build-speedups-for-ruby-3-2/
export RUBY_YJIT_ENABLE=true

##########
# python #
##########

# assumes that rye has been installed via asdf

source "$HOME/.rye/env"

#######
# zed #
#######

export EDITOR="zed-preview --wait"

############
# Orbstack #
############

# Added by OrbStack: command-line tools and integration
# Comment this line if you don't want it to be added again.
source ~/.orbstack/shell/init.zsh 2>/dev/null || :

########
# mise #
########

eval "$(mise activate zsh --shims)"

########
# pnpm #
########

export PNPM_HOME="$HOME/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

#############
# Extempore #
#############

export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"
