#!/usr/bin/env bash

# create an ed25519 key with FILENAME and COMMENT in ~/.ssh
# USAGE: make-ssh-key.sh filename comment

ssh-keygen -f ~/.ssh/$1 -t ed25519 -C "$2"
