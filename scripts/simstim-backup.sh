#!/bin/bash

#############################
# Ben Swift's Backup Script #
#############################

SIMSTIM=mai.anu.edu.au
BACKUP_DIR=/home/ben/hodgey-backup/

echo "Backing up to simstim: $SIMSTIM"

# code and docs

rsync -az \
      -e ssh \
      --delete \
      --stats \
      --human-readable \
      --exclude 'Library/Caches/' \
      --exclude 'Library/Preferences/SDMHelpData/' \
      --exclude 'Library/Mobile Documents.*' \
      --exclude '.Trash/' \
      --exclude 'Music/iTunes/iTunes\ Media/Mobile\ Applications/' \
      ~/ ben@$SIMSTIM:$BACKUP_DIR
