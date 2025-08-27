#!/bin/bash
# Wrapper to store stdin to macOS keychain
# Usage: echo "data" | keychain-store.sh account service

ACCOUNT="$1"
SERVICE="$2"
DATA=$(cat)

security add-generic-password -U -a "$ACCOUNT" -s "$SERVICE" -w "$DATA"
