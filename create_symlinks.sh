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

    # Remove existing symlink or file
    if [ -e "$target" ] || [ -L "$target" ]; then
        rm "$target"
    fi

    # Create new symlink
    ln -s "$source_path" "$target"
    echo "Created symlink: $target -> $source_path"
done
