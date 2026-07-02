#!/usr/bin/env bash

set -euo pipefail

# Configuration
DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
DRY_RUN="${DRY_RUN:-false}"

source "$DOTFILES_DIR/lib/log.sh"
source "$DOTFILES_DIR/lib/symlink-manifest.sh"

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

create_symlink_with_backup() {
    local source_path="$1"
    local target="$2"
    local timestamp

    if [[ -e "$target" && ! -L "$target" ]]; then
        if cmp -s "$source_path" "$target"; then
            if [[ "$DRY_RUN" == "true" ]]; then
                echo -e "${YELLOW}[DRY RUN] Would replace identical file with symlink: $target${NC}"
            else
                rm "$target"
            fi
        else
            timestamp="$(date +%Y%m%d%H%M%S)"
            if [[ "$DRY_RUN" == "true" ]]; then
                echo -e "${YELLOW}[DRY RUN] Would back up existing file: $target -> $target.backup.$timestamp${NC}"
            else
                mv "$target" "$target.backup.$timestamp"
            fi
        fi
    fi

    create_symlink "$source_path" "$target"
}

main() {
    echo "Setting up dotfiles symlinks..."
    echo "Dotfiles directory: $DOTFILES_DIR"

    if [[ "$DRY_RUN" == "true" ]]; then
        echo -e "${YELLOW}Running in DRY RUN mode - no changes will be made${NC}"
    fi

    local entry

    echo -e "\nLinking manifest entries..."
    for entry in "${SYMLINK_MANIFEST[@]}"; do
        create_symlink "$(manifest_source "$entry")" "$(manifest_target "$entry")"
    done

    echo -e "\nLinking tool-rewritten configs (with backup)..."
    for entry in "${SYMLINK_MANIFEST_WITH_BACKUP[@]}"; do
        create_symlink_with_backup "$(manifest_source "$entry")" "$(manifest_target "$entry")"
    done

    echo -e "\nLinking nb plugins..."
    while IFS= read -r entry; do
        create_symlink "$(manifest_source "$entry")" "$(manifest_target "$entry")"
    done < <(nb_plugin_links)

    echo -e "\n${GREEN}Done!${NC}"
}

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
    --dry-run | -n)
        DRY_RUN=true
        shift
        ;;
    --help | -h)
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
