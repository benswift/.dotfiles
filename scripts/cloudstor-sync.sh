#!/bin/bash

rclone sync --progress --exclude-from ~/.dotfiles/scripts/cloudstor-exclude-files.txt ~/Documents cloudstor:mitch-rclone/Documents
