# mise shims first for non-interactive contexts (IDEs, MCP servers); mise activate in zshrc takes precedence in shells
export PATH="$HOME/.local/share/mise/shims:$HOME/.dotfiles/bin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.bun/bin:$PATH"
export RUBY_YJIT_ENABLE=true
export EDITOR="zed-preview --wait"
export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"
