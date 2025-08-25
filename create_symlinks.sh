#!/bin/bash

# Files to link in home directory
files=(
    "RProfile"
    "gitconfig"
    "gitignore"
    "mbsyncrc"
    "notmuch-config"
    "zshenv"
    "zshrc"
)

# Zed config files to link
zed_files=(
    "keymap.json"
    "settings.json"
    "tasks.json"
)

# Get the absolute path of the .dotfiles directory
DOTFILES_DIR="$HOME/.dotfiles"

# Create symlinks in home directory
for file in "${files[@]}"; do
    target="$HOME/.${file}"
    source_path="$DOTFILES_DIR/$file"

    # Create symlink (overwriting if exists)
    ln -sfn "$source_path" "$target"
    echo "Created symlink: $target -> $source_path"
done

# Create .config/zed directory if it doesn't exist
mkdir -p "$HOME/.config/zed"

# Create symlinks for zed config files
for file in "${zed_files[@]}"; do
    target="$HOME/.config/zed/${file}"
    source_path="$DOTFILES_DIR/zed/${file}"

    # Create symlink (overwriting if exists)
    ln -sfn "$source_path" "$target"
    echo "Created symlink: $target -> $source_path"
done

# Create ~/.claude directory if it doesn't exist
mkdir -p "$HOME/.claude"
mkdir -p "$HOME/.claude/agents"

# Symlink individual Claude config files
claude_config_files=(
    "CLAUDE.md"
    "settings.json"
)

for file in "${claude_config_files[@]}"; do
    if [ -f "$DOTFILES_DIR/claude/$file" ]; then
        target="$HOME/.claude/$file"
        source_path="$DOTFILES_DIR/claude/$file"
        ln -sfn "$source_path" "$target"
        echo "Created symlink: $target -> $source_path"
    fi
done

# Symlink agent files
if [ -d "$DOTFILES_DIR/claude/agents" ]; then
    for agent in "$DOTFILES_DIR/claude/agents"/*; do
        if [ -f "$agent" ]; then
            target="$HOME/.claude/agents/$(basename "$agent")"
            ln -sfn "$agent" "$target"
            echo "Created symlink: $target -> $agent"
        fi
    done
fi

# Create Library/Preferences directory if it doesn't exist (though it should always exist on macOS)
mkdir -p "$HOME/Library/Preferences"

# Create symlink for entire aerc directory (macOS location)
target="$HOME/Library/Preferences/aerc"
source_path="$DOTFILES_DIR/aerc"

# Create symlink to aerc directory (using -n to avoid recursive symlinks)
ln -sfn "$source_path" "$target"
echo "Created symlink: $target -> $source_path"

# Create .config directory if it doesn't exist
mkdir -p "$HOME/.config"

# Create symlink for entire notmuch directory
target="$HOME/.config/notmuch"
source_path="$DOTFILES_DIR/notmuch"

# Create symlink to notmuch directory (using -n to avoid recursive symlinks)
ln -sfn "$source_path" "$target"
echo "Created symlink: $target -> $source_path"
