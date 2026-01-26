set -o vi

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
cpath() { realpath "$1" | pbcopy && echo "Copied to clipboard: $(realpath "$1")"; }

source ~/.orbstack/shell/init.zsh 2>/dev/null || :

if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

if [[ -n $HOMEBREW_PREFIX ]]; then
  FPATH="$HOMEBREW_PREFIX/share/zsh/site-functions:$FPATH"
fi

# Initialize completions
autoload -Uz compinit
compinit

eval "$(mise activate zsh)"
eval "$(zoxide init zsh)"

[[ -f $HOMEBREW_PREFIX/opt/spaceship/spaceship.zsh ]] && source "$HOMEBREW_PREFIX/opt/spaceship/spaceship.zsh"
