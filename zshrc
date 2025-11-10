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
alias bu="brew upgrade"
alias task="backlog task"
alias todo="nb todo undone"
alias latest="nb --limit 10"
# alias sc='screencapture -i -t jpg screencap.jpg'
alias update-usage-rules='mix usage_rules.sync AGENTS.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing'
alias neomutt='TERM=xterm-direct neomutt'
alias prettify-md='prettier --prose-wrap always --write "**/*.md"'
alias zed-update-expert='rm -rf "$HOME/Library/Application Support/Zed/extensions/work/elixir/expert-nightly" && echo "Expert cache cleared. Restart Zed to download latest."'

source ~/.orbstack/shell/init.zsh 2>/dev/null || :

if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
  # Add Homebrew completions to FPATH
  FPATH="$HOMEBREW_PREFIX/share/zsh/site-functions:$FPATH"
fi

# Initialize completions
autoload -Uz compinit
compinit

eval "$(mise activate zsh)"
eval "$(zoxide init zsh)"

# this needs to be set after mise activate zsh
if [ "$HOST" = "weddle" ] || [ "$(hostname)" = "weddle" ]; then
  # On weddle: black text on colored backgrounds for SSH sessions
  export PS1='%K{red}[%D{%H:%M}]%k%f %K{cyan}%m%k%f:%F{green}%1~%f %F{%(?.green.red)}%(!.#.$)%f '
else
  # Default prompt for other machines
  export PS1='%F{magenta}[%D{%H:%M}]%f %F{cyan}%m%f:%F{green}%1~%f %F{%(?.green.red)}%(!.#.$)%f '
fi
