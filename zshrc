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
alias claude-yolo="claude --dangerously-skip-permissions"
alias codex-yolo="codex --dangerously-bypass-approvals-and-sandbox"
alias gemini-yolo="gemini --yolo"
sysup() {
  if [[ "$OSTYPE" == darwin* ]]; then
    brew upgrade
  else
    sudo apt update && sudo apt upgrade
  fi
  mise upgrade
  uv tool upgrade --all
}
alias task="backlog task"
alias todo="nb todo undone"
alias latest="nb --limit 10"
# alias sc='screencapture -i -t jpg screencap.jpg'
alias update-usage-rules='mix usage_rules.sync AGENTS.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing'
alias neomutt='TERM=xterm-direct neomutt'
alias prettify-md='prettier --prose-wrap always --write "**/*.md"'
# macOS-only aliases
if [[ "$OSTYPE" == darwin* ]]; then
  alias zed-update-expert='rm -rf "$HOME/Library/Application Support/Zed/extensions/work/elixir/expert-nightly" && echo "Expert cache cleared. Restart Zed to download latest."'
fi
# Platform-specific clipboard command
if command -v pbcopy &>/dev/null; then
  clipboard() { pbcopy; }
elif command -v xclip &>/dev/null; then
  clipboard() { xclip -selection clipboard; }
elif command -v wl-copy &>/dev/null; then
  clipboard() { wl-copy; }
else
  clipboard() { cat > /dev/null; echo "No clipboard tool available" >&2; return 1; }
fi
cpath() { realpath "$1" | clipboard && echo "Copied to clipboard: $(realpath "$1")"; }

# macOS-specific integrations
[[ -f ~/.orbstack/shell/init.zsh ]] && source ~/.orbstack/shell/init.zsh

# Homebrew setup (macOS Apple Silicon, macOS Intel, or Linux)
if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -f /usr/local/bin/brew ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
elif [[ -f /home/linuxbrew/.linuxbrew/bin/brew ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

if [[ -n $HOMEBREW_PREFIX ]]; then
  FPATH="$HOMEBREW_PREFIX/share/zsh/site-functions:$FPATH"
fi

# Initialize completions
autoload -Uz compinit
compinit

eval "$(mise activate zsh)"
eval "$(zoxide init zsh)"

eval "$(starship init zsh)"
