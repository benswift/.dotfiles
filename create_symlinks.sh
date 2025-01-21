#!/bin/bash

# Files to link
files=(
    "RProfile"
    "gitconfig"
    "gitignore"
    "zshenv"
    "zshrc"
)

# Get the absolute path of the .dotfiles directory
DOTFILES_DIR="$HOME/.dotfiles"

# Create symlinks
for file in "${files[@]}"; do
    target="$HOME/.${file}"
    source_path="$DOTFILES_DIR/$file"

    # Create symlink (overwriting if exists)
    ln -sf "$source_path" "$target"
    echo "Created symlink: $target -> $source_path"
done
