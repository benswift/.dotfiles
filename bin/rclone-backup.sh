#!/bin/bash

set -euo pipefail

# Configuration
SOURCE_DIR="${RCLONE_SOURCE:-$HOME/Documents}"
REMOTE="${RCLONE_REMOTE:-weddle}"
REMOTE_PATH="${RCLONE_REMOTE_PATH:-backup/mitch/Documents}"
DRY_RUN="${DRY_RUN:-false}"

# Exclude patterns for build artifacts and regeneratable content
EXCLUDES=(
  ".git/**"
  "node_modules/**"
  ".venv/**"
  "venv/**"
  "_build/**"
  "deps/**"
  ".elixir_ls/**"
  ".expert/**"
  ".tox/**"
  "target/**"
  "build/**"
  "llvm/**"
  "__pycache__/**"
  ".pytest_cache/**"
  "*.egg-info/**"
  ".DS_Store"
  ".cache/**"
  "CLAUDE.md"
  "codex.md"
)

usage() {
  cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Sync ~/Documents to remote storage using rclone.

Options:
  -n, --dry-run    Show what would be transferred without making changes
  -r, --remote     Set remote name (default: weddle)
  -h, --help       Show this help message

Environment variables:
  RCLONE_SOURCE       Source directory (default: ~/Documents)
  RCLONE_REMOTE       Remote name (default: weddle)
  RCLONE_REMOTE_PATH  Remote path (default: backup/mitch/Documents)
  DRY_RUN             Set to 'true' for dry run mode

Excludes:
  .git/, node_modules/, .venv/, venv/, _build/, deps/, .elixir_ls/, .expert/,
  .tox/, target/, build/, llvm/, __pycache__/, .pytest_cache/, *.egg-info/,
  .DS_Store, .cache/, CLAUDE.md, codex.md
EOF
}

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--dry-run)
      DRY_RUN=true
      shift
      ;;
    -r|--remote)
      REMOTE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      usage
      exit 1
      ;;
  esac
done

# Build exclude arguments
EXCLUDE_ARGS=()
for pattern in "${EXCLUDES[@]}"; do
  EXCLUDE_ARGS+=(--exclude "$pattern")
done

# Build rclone command
RCLONE_ARGS=(
  sync
  --progress
  "${EXCLUDE_ARGS[@]}"
)

if [[ "$DRY_RUN" == "true" ]]; then
  RCLONE_ARGS+=(--dry-run)
  echo "DRY RUN MODE - no changes will be made"
fi

echo "Syncing $SOURCE_DIR to $REMOTE:$REMOTE_PATH"
echo "Excluding: ${EXCLUDES[*]}"
echo

rclone "${RCLONE_ARGS[@]}" "$SOURCE_DIR" "$REMOTE:$REMOTE_PATH"
