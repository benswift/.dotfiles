#!/usr/bin/env bash

# create an ed25519 key with FILENAME and COMMENT in ~/.ssh
# USAGE: make-ssh-key.sh filename comment

set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "Usage: $(basename "$0") filename comment" >&2
  exit 1
fi

filename=$1
comment=$2

ssh-keygen -f "$HOME/.ssh/${filename}" -t ed25519 -C "$comment"
