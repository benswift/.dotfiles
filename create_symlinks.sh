#!/bin/bash

set -euo pipefail

# Configuration
DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
DRY_RUN="${DRY_RUN:-false}"

# Colour output for better readability
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No colour

# Helper function to create symlinks with error handling
create_symlink() {
    local source_path="$1"
    local target="$2"
    local parent_dir

    # Check if source exists
    if [[ ! -e "$source_path" ]]; then
        echo -e "${RED}✗ Source not found: $source_path${NC}"
        return 1
    fi

    # Create parent directory if needed
    parent_dir="$(dirname "$target")"
    if [[ ! -d "$parent_dir" ]]; then
        if [[ "$DRY_RUN" == "true" ]]; then
            echo -e "${YELLOW}[DRY RUN] Would create directory: $parent_dir${NC}"
        else
            mkdir -p "$parent_dir"
        fi
    fi

    # Check if target exists and is not a symlink
    if [[ -e "$target" && ! -L "$target" ]]; then
        echo -e "${RED}✗ Target exists and is not a symlink: $target${NC}"
        echo "  Consider backing up and removing it first"
        return 1
    fi

    # Create the symlink
    if [[ "$DRY_RUN" == "true" ]]; then
        echo -e "${YELLOW}[DRY RUN] Would link: $target -> $source_path${NC}"
    else
        ln -sfn "$source_path" "$target"
        echo -e "${GREEN}✓ Linked: $target -> $source_path${NC}"
    fi
}

# Process a list of files with a common pattern
link_files() {
    local source_dir="$1"
    local target_dir="$2"
    local prefix="${3:-}"
    local suffix="${4:-}"
    shift 4
    local files=("$@")

    for file in "${files[@]}"; do
        local source_path="$source_dir/$file"
        local target="$target_dir/${prefix}${file}${suffix}"
        create_symlink "$source_path" "$target"
    done
}

# Process directory symlinks
link_directory() {
    local source_path="$1"
    local target="$2"

    if [[ ! -d "$source_path" ]]; then
        echo -e "${RED}✗ Source directory not found: $source_path${NC}"
        return 1
    fi

    create_symlink "$source_path" "$target"
}

main() {
    echo "Setting up dotfiles symlinks..."
    echo "Dotfiles directory: $DOTFILES_DIR"

    if [[ "$DRY_RUN" == "true" ]]; then
        echo -e "${YELLOW}Running in DRY RUN mode - no changes will be made${NC}"
    fi

    # Dotfiles in home directory (with . prefix)
    local home_files=(
        "RProfile"
        "gitconfig"
        "gitignore"
        "notmuch-config"
        "zshenv"
        "zshrc"
    )

    echo -e "\nLinking home directory dotfiles..."
    link_files "$DOTFILES_DIR" "$HOME" "." "" "${home_files[@]}"
    
    # Mail config files
    echo -e "\nLinking mail config files..."
    create_symlink "$DOTFILES_DIR/mail/mbsyncrc" "$HOME/.mbsyncrc"
    create_symlink "$DOTFILES_DIR/mail/msmtprc" "$HOME/.msmtprc"

    # Zed config files
    local zed_files=(
        "keymap.json"
        "settings.json"
        "tasks.json"
    )

    echo -e "\nLinking Zed config files..."
    link_files "$DOTFILES_DIR/zed" "$HOME/.config/zed" "" "" "${zed_files[@]}"

    # Claude config files
    local claude_files=(
        "CLAUDE.md"
        "settings.json"
    )

    echo -e "\nLinking Claude config files..."
    link_files "$DOTFILES_DIR/claude" "$HOME/.claude" "" "" "${claude_files[@]}"

    # Directory symlinks
    echo -e "\nLinking directories..."
    link_directory "$DOTFILES_DIR/claude/agents" "$HOME/.claude/agents"
    link_directory "$DOTFILES_DIR/aerc" "$HOME/Library/Preferences/aerc"
    link_directory "$DOTFILES_DIR/notmuch" "$HOME/.config/notmuch"
    link_directory "$DOTFILES_DIR/mail/neomutt" "$HOME/.config/neomutt"

    echo -e "\n${GREEN}Done!${NC}"
}

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run|-n)
            DRY_RUN=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  -n, --dry-run    Show what would be done without making changes"
            echo "  -h, --help       Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  DOTFILES_DIR     Set the dotfiles directory (default: ~/.dotfiles)"
            echo "  DRY_RUN          Set to 'true' for dry run mode"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

main
