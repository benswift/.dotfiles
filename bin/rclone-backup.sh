#!/usr/bin/env bash

set -euo pipefail

# Configuration
REMOTE="${RCLONE_REMOTE:-weddle}"
REMOTE_BASE="${RCLONE_REMOTE_BASE:-backup/mitch}"
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

Sync ~/Documents and ~/Maildir to remote storage using rclone.

Options:
  -n, --dry-run    Show what would be transferred without making changes
  -r, --remote     Set remote name (default: weddle)
  -h, --help       Show this help message

Environment variables:
  RCLONE_REMOTE       Remote name (default: weddle)
  RCLONE_REMOTE_BASE  Remote base path (default: backup/mitch)
  DRY_RUN             Set to 'true' for dry run mode
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

# Build exclude arguments for Documents
EXCLUDE_ARGS=()
for pattern in "${EXCLUDES[@]}"; do
  EXCLUDE_ARGS+=(--exclude "$pattern")
done

BASE_ARGS=(sync --progress)
if [[ "$DRY_RUN" == "true" ]]; then
  BASE_ARGS+=(--dry-run)
  echo "DRY RUN MODE - no changes will be made"
fi

echo "=== Documents ==="
echo "Syncing $HOME/Documents to $REMOTE:$REMOTE_BASE/Documents"
echo "Excluding: ${EXCLUDES[*]}"
echo
rclone "${BASE_ARGS[@]}" "${EXCLUDE_ARGS[@]}" "$HOME/Documents" "$REMOTE:$REMOTE_BASE/Documents"

echo
echo "=== Maildir ==="
echo "Syncing $HOME/Maildir to $REMOTE:Maildir"
echo
rclone "${BASE_ARGS[@]}" --exclude ".DS_Store" "$HOME/Maildir" "$REMOTE:Maildir"
