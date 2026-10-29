#!/bin/zsh
# Read by every zsh, interactive or not (scripts, `ssh host cmd`, agent
# shells), so this is where PATH is built, as the zsh manual prescribes. Keep
# it cheap and silent: no output, no subprocesses, nothing that assumes a tty.
# Interactive setup belongs in zshrc.
#
# zprofile sources this file again: on macOS /etc/zprofile runs path_helper
# after this file, which pushes every dir below behind /usr/bin. Re-running
# restores the order, and `typeset -U` drops the duplicates that leaves, and
# the ones nested shells (zellij panes) would pile up.

typeset -U path

# Static rather than `eval "$(brew shellenv)"`, which forks a bash script
# on every zsh start. HOMEBREW_PREFIX is all zshrc needs (completions).
if [[ -d /opt/homebrew ]]; then
  export HOMEBREW_PREFIX=/opt/homebrew
  path=(/opt/homebrew/bin /opt/homebrew/sbin $path)
fi

# ~/.wiz/bin is the collaborators' install of the wiz the lx vault pins. It
# sits behind ~/.local/bin so bin/wiz-build's source build wins wherever there
# is one; watson has none, and runs the pinned wiz.
path=(~/.dotfiles/bin ~/.dotfiles/private/bin(N) ~/.local/bin ~/.wiz/bin(N) ~/.cargo/bin ~/.bun/bin $path ~/.orbstack/bin(N))

# mise's shims, appended last. `mise activate` only runs from .zshrc, so
# without this a non-interactive shell sees none of the mise-managed tools ---
# which is how a yazi launched outside a prompt lost tree-sitter and silently
# previewed everything through bat. The systemd units in systemd/user/ already
# put this directory on PATH for the same reason. Appending (not prepending)
# keeps activate's real bin dirs first in an interactive shell, so the shims
# cost nothing there; each shim exec re-enters mise, at roughly 30ms.
path+=(~/.local/share/mise/shims)

export RUBY_YJIT_ENABLE=true
export EDITOR="hx"
export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"
