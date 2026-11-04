#!/bin/zsh

set -o vi

# vi-mode rebinds: use readline-style backspace/word-kill rather than vi's
# stricter variants that won't cross the insert-start boundary
bindkey -M viins '^?' backward-delete-char
bindkey -M viins '^H' backward-delete-char
bindkey -M viins '^W' backward-kill-word
bindkey -M viins '^U' backward-kill-line

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
alias ay="claude --dangerously-skip-permissions"
alias aym="claude --dangerously-skip-permissions --effort max"
# --setting-sources skips project .claude/settings.json (comp4020 pins the ANU
# proxy there), so the Max login is used; local/user settings still load.
alias ayf="claude --setting-sources user,local --dangerously-skip-permissions --model fable --effort high"
alias oy="command codex --dangerously-bypass-approvals-and-sandbox"
alias gyolo="gemini --yolo"
sysup() {
  if [[ "$OSTYPE" == darwin* ]]; then
    HOMEBREW_NO_UPGRADE_AUTO_UPDATES_CASKS=1 brew upgrade --formula --yes
  else
    sudo apt update && sudo apt upgrade -y
    mise self-update -y
  fi
  mise plugin update
  mise upgrade -y
  uv tool upgrade --all
  bun update -g --latest
  cargo install-update -a
  dotfiles update
}
alias task="backlog task"
# todo (bin/todo): list / do / block, or bare words for a new task
# A titled note in the anu vault, opened in the editor; with text on stdin,
# that's its body instead: `note Call with Ann < minutes.md`.
note() {
  [[ $# -gt 0 ]] || { echo "usage: note <title words>" >&2; return 2; }
  local body="" added
  [[ -t 0 ]] || body="$(cat)"
  added="$(printf '%s' "$body" | wiz add anu note -json -title "$*" | jq -r '.paths[0]')" || return
  [[ -n "$body" ]] && echo "$added" || "${EDITOR:-hx}" "$(wiz path "anu:$added")"
}
alias latest='wiz query anu "path.startsWith(\"notes/\")" | tail -10'
# Copy a vault file's full path: `cpath ben:notes/2026-10-29-x.md`.
cpath() { wiz path "$1" | tr -d '\n' | clipboard-copy && echo "copied $(wiz path "$1")"; }
# alias sc='screencapture -i -t jpg screencap.jpg'
alias update-usage-rules='mix usage_rules.sync CLAUDE.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing'
# git shortcuts
alias gst="git status"
alias gd="git diff"
alias gl="git log --oneline"
alias gsy="git sync"  # bin/git-sync
# zellij shortcuts
zs() { zellij --session "${1:-${PWD##*/}}" "${@:2}"; }
alias za="zellij attach"
zl() { local out; out=$(zellij list-sessions); echo "$out" | grep -v EXITED; echo; echo "$out" | grep EXITED; }
alias zr="zellij run --"
alias zk="zellij kill-session"
alias prettify-md='oxfmt -c ~/.dotfiles/oxfmtrc.json "**/*.md"'
alias nm='neomutt'
# Clipboard via OSC 52 (works locally and over SSH in ghostty); see
# bin/clipboard-copy for the fallback chain when there's no terminal
clipboard() { clipboard-copy; }
cpath() { realpath "${1:-.}" | clipboard && echo "Copied to clipboard: $(realpath "${1:-.}")"; }

# Completions from Homebrew and OrbStack (macOS); their bin dirs are on PATH
# via zshenv
[[ -n $HOMEBREW_PREFIX ]] && fpath=($HOMEBREW_PREFIX/share/zsh/site-functions $fpath)
[[ -d ~/.orbstack/shell/completions/zsh ]] && fpath+=(~/.orbstack/shell/completions/zsh)

# Initialize completions. The full compinit audits every completion file,
# which is the slowest part of startup --- only do that when the dump is
# more than a day old, otherwise trust it with -C.
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

eval "$(mise activate zsh)"
eval "$(zoxide init zsh)"

case "$(hostname -s)" in
  daysy)       _prompt_host_color='%F{yellow}' ;;
  weddle)      _prompt_host_color='%F{green}' ;;
  cybersonic)  _prompt_host_color='%F{magenta}' ;;
  jetson-orin) _prompt_host_color='%F{cyan}' ;;
  strproxy)    _prompt_host_color='%F{9}' ;;
  *)           _prompt_host_color='%F{9}' ;;
esac

_prompt_precmd() {
  local exit_status=$?
  local sep_color='%F{8}'
  local dir_color='%F{blue}'
  local vcs_color='%F{magenta}'

  print -P "${sep_color}${(l:$COLUMNS::─:)}%f"

  local vcs
  vcs=$(git branch --show-current 2>/dev/null)

  local host=""
  [[ -n "$_prompt_host_color" ]] && host="%B${_prompt_host_color}%m%f%b "

  PROMPT="${host}%B${dir_color}%1~%f%b${vcs:+ %B${vcs_color}${vcs}%b%f} %(?.%F{green}.%F{red})❯%f "
}
autoload -Uz add-zsh-hook
add-zsh-hook precmd _prompt_precmd
