#!/bin/bash
# Archive cleanup script for ANU mailbox
# Processes deletions in small batches to avoid timeout/auth issues

ARCHIVE_DIR="$HOME/Maildir/anu/Archive/cur"
BATCH_SIZE=5000
LOG_FILE="$HOME/archive_cleanup.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "==================================================" | tee -a "$LOG_FILE"
echo "Archive Cleanup Script Started: $(date)" | tee -a "$LOG_FILE"
echo "==================================================" | tee -a "$LOG_FILE"

# Function to count files
count_files() {
    local total=$(ls -1 "$ARCHIVE_DIR" | wc -l | tr -d ' ')
    local trash=$(ls -1 "$ARCHIVE_DIR" | grep ':2,.*T' | wc -l | tr -d ' ')
    local keep=$(ls -1 "$ARCHIVE_DIR" | grep -v ':2,.*T' | wc -l | tr -d ' ')
    
    echo "Current state:" >&2
    echo "  Total files: $total" >&2
    echo "  Marked for deletion: $trash" >&2
    echo "  Files to keep: $keep" >&2
    echo "" >&2
    
    echo "$(date): Total=$total, Trash=$trash, Keep=$keep" >> "$LOG_FILE"
    
    echo "$trash"  # Return trash count only
}

# Initial count
echo -e "${YELLOW}Initial state:${NC}"
trash_count=$(count_files)

if [ "$trash_count" -eq 0 ]; then
    echo -e "${GREEN}No files marked for deletion. Exiting.${NC}"
    exit 0
fi

# Process in batches
batch_num=0
total_deleted=0

while [ "$trash_count" -gt 0 ]; do
    batch_num=$((batch_num + 1))
    
    # Calculate batch
    if [ "$trash_count" -lt "$BATCH_SIZE" ]; then
        current_batch=$trash_count
    else
        current_batch=$BATCH_SIZE
    fi
    
    echo -e "${YELLOW}Batch $batch_num: Processing $current_batch files...${NC}"
    echo "$(date): Starting batch $batch_num ($current_batch files)" >> "$LOG_FILE"
    
    # Get files to delete in this batch
    cd "$ARCHIVE_DIR"
    files_to_delete=$(ls -1 | grep ':2,.*T' | head -n "$current_batch")
    
    # Delete the batch locally first
    echo "  Removing $current_batch files locally..."
    for file in $files_to_delete; do
        rm -f "$file"
    done
    
    echo "  Syncing with server..."
    
    # Sync with server (with timeout)
    if timeout 300 mbsync --expunge anu:Archive 2>&1 | tee -a "$LOG_FILE"; then
        echo -e "${GREEN}  Batch $batch_num completed successfully${NC}"
        total_deleted=$((total_deleted + current_batch))
    else
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo -e "${RED}  Sync timed out after 5 minutes${NC}" | tee -a "$LOG_FILE"
        else
            echo -e "${RED}  Sync failed with error code $exit_code${NC}" | tee -a "$LOG_FILE"
        fi
        
        echo -e "${YELLOW}Do you want to continue? (y/n)${NC}"
        read -r response
        if [ "$response" != "y" ]; then
            echo "Cleanup stopped by user" | tee -a "$LOG_FILE"
            break
        fi
    fi
    
    # Re-count
    echo ""
    echo "After batch $batch_num:"
    trash_count=$(count_files)
    
    # Small delay between batches
    if [ "$trash_count" -gt 0 ]; then
        echo "Waiting 2 seconds before next batch..."
        sleep 2
    fi
    
    echo ""
done

echo "==================================================" | tee -a "$LOG_FILE"
echo -e "${GREEN}Cleanup Complete!${NC}" | tee -a "$LOG_FILE"
echo "Total files deleted: $total_deleted" | tee -a "$LOG_FILE"
echo "Final state:"
count_files
echo "==================================================" | tee -a "$LOG_FILE"
echo "Log saved to: $LOG_FILE"