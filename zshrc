set -o emacs

alias zp="zed-preview"

source ~/.orbstack/shell/init.zsh 2>/dev/null || :

if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi
eval "$(mise activate zsh)"

# source "$HOME/.rye/env"
