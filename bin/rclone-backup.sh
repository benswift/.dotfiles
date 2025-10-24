#!/bin/bash

set -euo pipefail

EXCLUDE_FROM_FILE="$(mktemp -t rclone-excludes.XXXXXX)"
CLONE_ROOT_DIR="${CLONE_ROOT_DIR:-$HOME/Documents}"

## visit it at https://anu365-my.sharepoint.com/personal/u2548636_anu_edu_au/PreservationHoldLibrary
REMOTE=anu-onedrive

cleanup() {
  rm -f "$EXCLUDE_FROM_FILE"
}
trap cleanup EXIT

## find all git repo enclosing folders (including trailing slash), munge them
## into the form that rclone expects for its "exclude from" file
echo "finding git repos to ignore..."
: > "$EXCLUDE_FROM_FILE"
(cd "$CLONE_ROOT_DIR" && find . -type d -name .git -print0) | \
  while IFS= read -r -d '' git_dir; do
    repo_dir=$(dirname "$git_dir")
    printf '%s/\n' "${repo_dir#./}" >> "$EXCLUDE_FROM_FILE"
  done

## add a few extra excludes
echo ".DS_Store" >> "$EXCLUDE_FROM_FILE"
echo "ignoring files in $(wc -l < "$EXCLUDE_FROM_FILE" | tr -s ' ') git repos"

## sync to remote
echo "syncing to remote $REMOTE"
rclone sync --progress --exclude-from="$EXCLUDE_FROM_FILE" "$CLONE_ROOT_DIR" "$REMOTE:mitch-rclone${CLONE_ROOT_DIR}"
