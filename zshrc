#!/bin/zsh

set -o vi

zle-keymap-select() {
  case $KEYMAP in
    vicmd) printf '\e[2 q' ;;
    viins|main) printf '\e[6 q' ;;
  esac
}
zle -N zle-keymap-select

zle-line-init() { printf '\e[6 q'; }
zle -N zle-line-init

# History configuration
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

alias y="yazi"
alias h="hx ."
alias zp="zed-preview"
alias ayolo="claude --dangerously-skip-permissions --effort max"
alias oyolo="codex --dangerously-bypass-approvals-and-sandbox"
alias gyolo="gemini --yolo"
sysup() {
  if [[ "$OSTYPE" == darwin* ]]; then
    brew upgrade
  else
    sudo apt update && sudo apt upgrade -y
  fi
  mise plugin update
  mise upgrade
  uv tool upgrade --all
  bun update -g
  cargo install-update -a
  dotfiles update
}
alias task="backlog task"
todo() {
  if [[ "$1" == "add" && $# -eq 1 ]]; then
    nb add --filename "$(date +%Y%m%d%H%M%S).todo.md" --content "# [ ] " --edit
  elif [[ $# -eq 0 ]]; then
    nb todo undone
  else
    nb todo "$@"
  fi
}
daily() {
  if [[ $# -eq 0 ]]; then
    local filename="$(date +%Y%m%d).md"
    local path="$HOME/.nb/home/$filename"
    if [[ ! -f "$path" ]]; then
      nb add --filename "$filename" --content "# Daily $(date +%Y-%m-%d)"$'\n' >/dev/null
    fi
    nb edit "$filename"
  else
    nb daily "$@"
  fi
}
alias latest="nb --limit 10"
# alias sc='screencapture -i -t jpg screencap.jpg'
alias update-usage-rules='mix usage_rules.sync CLAUDE.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing'
# git shortcuts
alias gst="git status"
alias gd="git diff"
alias gl="git log --oneline"
gp() { git pull --rebase; }
gship() { git push; }
gsy() {
  if ! git diff-index --quiet HEAD -- 2>/dev/null; then
    echo "Uncommitted changes --- commit or stash first." >&2
    return 1
  fi
  local remote
  remote=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null) || {
    echo "No upstream tracking branch set." >&2
    return 1
  }
  git fetch || return 1
  local local_ref upstream_ref base_ref
  local_ref=$(git rev-parse @)
  upstream_ref=$(git rev-parse @{u})
  base_ref=$(git merge-base @ @{u})
  if [[ "$local_ref" == "$upstream_ref" ]]; then
    echo "Already up to date."
  elif [[ "$local_ref" == "$base_ref" ]]; then
    echo "Pulling (rebase)..."
    git rebase @{u} || { echo "Rebase conflict --- resolve manually." >&2; git rebase --abort; return 1; }
  elif [[ "$upstream_ref" == "$base_ref" ]]; then
    echo "Pushing..."
    git push
  else
    echo "Local and remote have diverged --- resolve manually." >&2
    return 1
  fi
}
# zellij shortcuts
zs() { zellij --session "${PWD##*/}" "$@"; }
alias za="zellij attach"
zl() { local out; out=$(zellij list-sessions); echo "$out" | grep -v EXITED; echo; echo "$out" | grep EXITED; }
alias zr="zellij run --"
alias zk="zellij kill-session"
alias prettify-md='oxfmt -c ~/.dotfiles/oxfmtrc.json "**/*.md"'
alias neomutt='TERM=xterm-direct neomutt'
alias nm='neomutt'
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
cpath() { realpath "${1:-.}" | clipboard && echo "Copied to clipboard: $(realpath "${1:-.}")"; }

# macOS-specific integrations
[[ -f ~/.orbstack/shell/init.zsh ]] && source ~/.orbstack/shell/init.zsh

# Homebrew setup (macOS only)
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
command -v fnox &>/dev/null && eval "$(command fnox activate zsh)"
export LS_COLORS="$(vivid generate catppuccin-mocha)"
eval "$(zoxide init zsh)"

_prompt_precmd() {
  local exit_status=$?
  local sep_color='%F{#45475a}'
  local time_color='%F{#a6adc8}'
  local host_color='%F{#fab387}'
  local dir_color='%F{#89b4fa}'
  local vcs_color='%F{#cba6f7}'

  print -P "${sep_color}${(l:$COLUMNS::─:)}%f"

  local vcs
  vcs=$(git branch --show-current 2>/dev/null)

  local host=""
  [[ "$(hostname -s)" != "daysy" ]] && host="%B${host_color}%m%f%b "

  PROMPT="${time_color}%T%f ${host}%B${dir_color}%1~%f%b${vcs:+ %B${vcs_color}${vcs}%b%f} %(?.%F{#a6e3a1}.%F{#f38ba8})❯%f "
}
autoload -Uz add-zsh-hook
add-zsh-hook precmd _prompt_precmd
