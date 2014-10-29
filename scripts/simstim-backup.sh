#!/bin/bash

#############################
# Ben Swift's Backup Script #
#############################

SIMSTIM=mai.anu.edu.au
BACKUP_DIR=/home/ben/hodgey-backup/

echo "Backing up to simstim: $SIMSTIM"

# echo -e "\nBacking up \033[0;32mMusic\033[0;00m..."
# rsync -avz -e ssh --delete --stats --human-readable --exclude='Music/iTunes/iTunes\ Media/Mobile\ Applications/' ~/Music ben@$SIMSTIM:$BACKUP_DIR/Music/ && \
# echo -e "\nBacking up \033[0;32mLibrary\033[0;00m..."
# rsync -avz -e ssh --delete --stats --human-readable ~/Library ben@$SIMSTIM:$BACKUP_DIR/Library/ && \
echo -e "\nBacking up \033[0;32mCalibre\033[0;00m..."
rsync -avz -e ssh --delete --stats --human-readable ~/Calibre\ Library ben@$SIMSTIM:$BACKUP_DIR/Calibre\ Library/ && \
echo -e "\nBacking up \033[0;32mCode\033[0;00m..."
rsync -avz -e ssh --delete --stats --human-readable ~/Code ben@$SIMSTIM:$BACKUP_DIR/Code/ && \
echo -e "\nBacking up \033[0;32mDesktop\033[0;00m..."
# rsync -avz -e ssh --delete --stats --human-readable ~/Desktop ben@$SIMSTIM:$BACKUP_DIR/Desktop/ && \
echo -e "\nBacking up \033[0;32mDocuments\033[0;00m..."
rsync -avz -e ssh --delete --stats --human-readable ~/Documents ben@$SIMSTIM:$BACKUP_DIR/Documents/ && \
echo -e "\nBacking up \033[0;32mMovies\033[0;00m..."
rsync -avz -e ssh --delete --stats --human-readable ~/Movies ben@$SIMSTIM:$BACKUP_DIR/Movies/ && \
echo -e "\nBacking up \033[0;32mPictures\033[0;00m..."
rsync -avz -e ssh --delete --stats --human-readable ~/Pictures ben@$SIMSTIM:$BACKUP_DIR/Pictures/ && \
