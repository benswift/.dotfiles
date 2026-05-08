#!/bin/zsh

export PATH="$HOME/.dotfiles/bin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.bun/bin:$PATH"
export RUBY_YJIT_ENABLE=true
export EDITOR="hx"
export EXT_MIDI_OUT_DEVICE_NAME="Maschine 2 Virtual Input"

# Caches and big artefacts live on /data (3.6 TB free) instead of /home (94% full)
export HF_HOME="/data/$USER/cache/huggingface"
export UV_CACHE_DIR="/data/$USER/cache/uv"
export STANDING_ORDERS_RUNS_DIR="/data/$USER/standing-orders/runs"
