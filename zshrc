set -o emacs

# History configuration
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

alias zp="zed-preview"
alias todo="nb todo"
alias screencap='screencapture -i -t jpg screencap.jpg'
alias update-usage-rules='mix usage_rules.sync CLAUDE.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing'

source ~/.orbstack/shell/init.zsh 2>/dev/null || :

if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

eval "$(mise activate zsh)"

# this needs to be set after mise activate zsh
export PS1='%F{magenta}[%D{%H:%M}]%f %F{cyan}%m%f:%F{green}%1~%f %F{%(?.green.red)}%(!.#.$)%f '
