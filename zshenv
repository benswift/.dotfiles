#!/bin/zsh

export PATH="$HOME/.dotfiles/bin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.bun/bin:$PATH"

# mise's shims, appended last. `mise activate` only runs from .zshrc, so
# without this a non-interactive shell sees none of the mise-managed tools ---
# which is how a yazi launched outside a prompt lost tree-sitter and silently
# previewed everything through bat. The systemd units in systemd/user/ already
# put this directory on PATH for the same reason. Appending (not prepending)
# keeps activate's real bin dirs first in an interactive shell, so the shims
# cost nothing there; each shim exec re-enters mise, at roughly 30ms.
export PATH="$PATH:$HOME/.local/share/mise/shims"
export RUBY_YJIT_ENABLE=true
export EDITOR="hx"
export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"
