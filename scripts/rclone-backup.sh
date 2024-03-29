#!/bin/bash

EXCLUDE_FROM_FILE="/tmp/rclone-excludes.txt"
CLONE_ROOT_DIR=~/Documents

## visit it at https://anu365-my.sharepoint.com/personal/u2548636_anu_edu_au/PreservationHoldLibrary
REMOTE=anu-onedrive

## find all git repo enclosing folders (including trailing slash), munge them
## into the form that rclone expects for its "exclude from" file
echo "finding git repos to ignore..."
cd $CLONE_ROOT_DIR && find . -type d -name .git -exec  dirname {} \; | sed -e 's/$/\//' -e 's/^.//' > $EXCLUDE_FROM_FILE

## add a few extra excludes
echo ".DS_Store" >> $EXCLUDE_FROM_FILE
echo "ignoring files in $(wc -l < $EXCLUDE_FROM_FILE | tr -s " ") git repos"

## sync to remote
echo "syncing to remote $REMOTE"
rclone sync --progress --exclude-from=$EXCLUDE_FROM_FILE $CLONE_ROOT_DIR $REMOTE:mitch-rclone/$CLONE_ROOT_DIR
