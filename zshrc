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
alias ayolo="claude --dangerously-skip-permissions"
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
}
alias task="backlog task"
alias todo="nb todo undone"
alias daily="nb daily"
alias latest="nb --limit 10"
# alias sc='screencapture -i -t jpg screencap.jpg'
alias update-usage-rules='mix usage_rules.sync AGENTS.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing'
# jj shortcuts
alias js="jj st"
alias jd="jj diff"
alias jl="jj log"
alias jn="jj new"
alias jQ="jj squash"
alias je="jj edit"
alias jb="jj bookmark"
jm() { jj describe -m "$*"; }
jp() { jj git fetch && jj rebase -d main@origin; }
jship() {
  local rev="@"
  if [[ "$(jj log -r @ --no-graph -T 'empty')" == "true" ]]; then
    rev="@-"
  fi
  jj bookmark set main -r "$rev" && jj git push
}
jjw() {
  local name="${1:?usage: jjw <name> [-r rev]}"
  shift
  local root="$(jj root)"
  local dir="$(dirname "$root")/$(basename "$root")-$name"
  jj workspace add --name "$name" "$dir" "$@"
  cd "$dir"
  ayolo
}
jjw-forget() {
  local name="${1:?usage: jjw-forget <name>}"
  local root="$(jj root)"
  local dir="$(dirname "$root")/$(basename "$root")-$name"
  jj workspace forget "$name"
  rm -rf "$dir"
}
# zellij shortcuts
zs() { zellij --session "${PWD##*/}" "$@"; }
alias za="zellij attach"
alias zl="zellij list-sessions"
alias zr="zellij run --"
alias zk="zellij kill-session"
alias prettify-md='prettier --prose-wrap always --write "**/*.md"'
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
export LS_COLORS="$(vivid generate gruvbox-light)"
eval "$(zoxide init zsh)"

# Pure zsh prompt (replaces starship). Uses jj-starship for VCS status.
# Shows: time, cwd, jj/git status, hostname (only if not daysy), separator line.
_prompt_precmd() {
  local exit_status=$?
  local sep_color='%F{#d5c4a1}'
  local time_color='%F{#7c6f64}'
  local host_color='%F{#af3a03}'
  local dir_color='%F{#076678}'
  local vcs_color='%F{#8f3f71}'

  print -P "${sep_color}${(l:$COLUMNS::─:)}%f"

  local vcs
  vcs=$(jj-starship prompt --no-color 2>/dev/null)

  local host=""
  [[ "$(hostname -s)" != "daysy" ]] && host="%B${host_color}%m%f%b "

  PROMPT="${time_color}%T%f ${host}%B${dir_color}%1~%f%b${vcs:+ %B${vcs_color}${vcs}%b%f} %(?.%F{#79740e}.%F{#9d0006})❯%f "
}
autoload -Uz add-zsh-hook
add-zsh-hook precmd _prompt_precmd
