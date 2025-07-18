set -o emacs

alias zp="zed-preview"

source ~/.orbstack/shell/init.zsh 2>/dev/null || :

if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

eval "$(mise activate zsh)"

# this needs to be set after mise activate zsh
export PS1='%F{magenta}[%D{%H:%M}]%f %F{cyan}%m%f:%F{green}%1~%f %F{%(?.green.red)}%(!.#.$)%f '

alias claude="~/.claude/local/claude"
