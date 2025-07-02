#!/bin/bash

# Files to link in home directory
files=(
    "RProfile"
    "gitconfig"
    "gitignore"
    "zshenv"
    "zshrc"
)

# Zed config files to link
zed_files=(
    "keymap.json"
    "settings.json"
    "tasks.json"
)

# Claude config files to link
claude_files=(
    "settings.json"
)

# Get the absolute path of the .dotfiles directory
DOTFILES_DIR="$HOME/.dotfiles"

# Create symlinks in home directory
for file in "${files[@]}"; do
    target="$HOME/.${file}"
    source_path="$DOTFILES_DIR/$file"

    # Create symlink (overwriting if exists)
    ln -sf "$source_path" "$target"
    echo "Created symlink: $target -> $source_path"
done

# Create .config/zed directory if it doesn't exist
mkdir -p "$HOME/.config/zed"

# Create symlinks for zed config files
for file in "${zed_files[@]}"; do
    target="$HOME/.config/zed/${file}"
    source_path="$DOTFILES_DIR/zed/${file}"

    # Create symlink (overwriting if exists)
    ln -sf "$source_path" "$target"
    echo "Created symlink: $target -> $source_path"
done

# Create .claude directory if it doesn't exist
mkdir -p "$HOME/.claude"

# Create symlinks for claude config files
for file in "${claude_files[@]}"; do
    target="$HOME/.claude/${file}"
    source_path="$DOTFILES_DIR/claude/${file}"

    # Create symlink (overwriting if exists)
    ln -sf "$source_path" "$target"
    echo "Created symlink: $target -> $source_path"
done
