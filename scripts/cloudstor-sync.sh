#!/bin/bash

rclone sync --progress --exclude /roam/db/ ~/Documents/org cloudstor:smithy-rclone/Documents/org
rclone sync --progress --exclude-from ~/.dotfiles/scripts/cloudstor-exclude-files.txt ~/Documents cloudstor:smithy-rclone/Documents
