#!/bin/bash
# Complete script to fix ANU Archive maildir duplication issue
# This script will deduplicate the Archive folder and sync with the server
# Uses batched operations to handle Office365 throttling

set -e  # Exit on error

echo "=================================================="
echo "ANU Archive Maildir Deduplication Script"
echo "=================================================="
echo ""
echo "Current state:"
echo "- Total files in Archive: ~92,197"
echo "- Unique messages: ~30,279"
echo "- Duplicate files to remove: ~60,000+"
echo ""
echo "This script will:"
echo "1. Check mbsync state health"
echo "2. Run deduplication in dry-run mode"
echo "3. Create a full backup (optional)"
echo "4. Mark duplicate files with T flag in batches"
echo "5. Sync changes with the server using retry logic"
echo ""

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check if deduplicate script exists
if [ ! -f "$SCRIPT_DIR/deduplicate_maildir.py" ]; then
    echo "Error: $SCRIPT_DIR/deduplicate_maildir.py not found"
    exit 1
fi

# Step 0: Health check
echo "Step 0: Checking mbsync state health..."
python3 "$SCRIPT_DIR/deduplicate_maildir.py" --check-health

if [ $? -ne 0 ]; then
    echo ""
    echo "ERROR: mbsync state appears corrupted (large journal file detected)"
    echo ""
    echo "To fix this, you need to:"
    echo "1. Backup the journal file:"
    echo "   mv ~/Maildir/anu/Archive/.mbsyncstate.journal ~/Maildir/anu/Archive/.mbsyncstate.journal.backup"
    echo ""
    echo "2. Reset sync state from server (this is safe, won't delete local files):"
    echo "   mbsync --pull anu:Archive"
    echo ""
    echo "3. Re-run this script"
    exit 1
fi

# Step 1: Run dry-run (if plan doesn't exist)
if [ ! -f ~/deduplication_plan.json ]; then
    echo ""
    echo "Step 1: Running dry-run analysis (this will take several minutes)..."
    python3 "$SCRIPT_DIR/deduplicate_maildir.py" --dry-run
    
    if [ $? -ne 0 ]; then
        echo "Error: Dry-run failed"
        exit 1
    fi
else
    echo ""
    echo "Step 1: Using existing deduplication plan"
    cat ~/deduplication_plan.json | grep "expected_remaining"
fi

echo ""
echo "Dry-run complete. Review the plan above."
echo "Expected result: ~92,197 files -> ~30,279 files"
echo ""
echo "IMPORTANT: This will mark ~60,000 files for deletion!"
echo ""
read -p "Continue with deduplication? (yes/no): " confirm

if [ "$confirm" != "yes" ]; then
    echo "Aborted by user"
    exit 0
fi

# Ask about processing mode
echo ""
echo "Processing options:"
echo "1. Batch mode (RECOMMENDED) - Process 1000 files at a time, safe for Office365"
echo "2. All at once - Process all 60,000+ files (may hit throttling)"
echo ""
read -p "Choose processing mode (1 or 2): " mode

if [ "$mode" == "1" ]; then
    # Batch mode
    echo ""
    echo "Step 2: Processing in batches..."
    echo "This will process files in batches of 1000"
    echo "You can stop and resume at any time"
    echo ""
    
    # Create initial backup
    read -p "Create backup before starting? (yes/no): " backup_confirm
    if [ "$backup_confirm" == "yes" ]; then
        echo "Creating backup (this will take a while)..."
        python3 "$SCRIPT_DIR/deduplicate_maildir.py" --execute --batch --batch-size 1
        # This will fail but create the backup
    fi
    
    # Process batches
    CONTINUE=true
    while [ "$CONTINUE" == "true" ]; do
        echo ""
        echo "Processing next batch..."
        python3 "$SCRIPT_DIR/deduplicate_maildir.py" --execute --batch --resume --no-backup
        
        if [ $? -ne 0 ]; then
            echo "Batch processing failed"
            exit 1
        fi
        
        # Check if more batches remain
        if grep -q '"remaining": 0' ~/deduplication_batch_state.json 2>/dev/null; then
            echo "All batches complete!"
            CONTINUE=false
        else
            echo ""
            read -p "Continue with next batch? (yes/no): " batch_continue
            if [ "$batch_continue" != "yes" ]; then
                echo "Batch processing paused. Run script again to resume."
                CONTINUE=false
            fi
        fi
    done
    
else
    # All at once mode
    echo ""
    echo "Step 2: Executing deduplication all at once..."
    echo "WARNING: This may hit Office365 throttling with 60,000+ files"
    echo ""
    
    python3 "$SCRIPT_DIR/deduplicate_maildir.py" --execute
    
    if [ $? -ne 0 ]; then
        echo "Error: Deduplication failed"
        echo "You may need to restore from backup"
        exit 1
    fi
fi

# Step 3: Verify the results
echo ""
echo "Step 3: Verifying deduplication..."
python3 "$SCRIPT_DIR/deduplicate_maildir.py" --verify

if [ $? -ne 0 ]; then
    echo "Warning: Verification showed unexpected file count"
    echo "Check the logs before proceeding"
    read -p "Continue anyway? (yes/no): " confirm2
    if [ "$confirm2" != "yes" ]; then
        exit 1
    fi
fi

# Count T-flagged files
T_FLAGGED=$(find ~/Maildir/anu/Archive/cur -type f -name '*:2,*T*' | wc -l)
echo ""
echo "Files marked with T flag: $T_FLAGGED"

if [ "$T_FLAGGED" -lt 1000 ]; then
    echo "WARNING: Only $T_FLAGGED files marked with T flag (expected ~60,000)"
    echo "Something may be wrong. Review before syncing."
    read -p "Continue anyway? (yes/no): " confirm3
    if [ "$confirm3" != "yes" ]; then
        exit 1
    fi
fi

# Step 4: Sync with server
echo ""
echo "Step 4: Syncing with server"
echo "This will DELETE the T-flagged messages on the server (due to 'Expunge Both' in mbsyncrc)"
echo "Office365 may throttle this operation - the retry script will handle it"
echo ""
read -p "Proceed with server sync and expunge? (yes/no): " sync_confirm

if [ "$sync_confirm" != "yes" ]; then
    echo "Server sync skipped. Local T-flag marking is complete."
    echo "To sync later, run: $SCRIPT_DIR/sync_with_retry.sh"
    exit 0
fi

# Use the retry script for syncing
if [ -f "$SCRIPT_DIR/sync_with_retry.sh" ]; then
    echo "Running sync with automatic retry and throttling support..."
    bash "$SCRIPT_DIR/sync_with_retry.sh"
else
    echo "Warning: sync_with_retry.sh not found, using plain mbsync"
    mbsync anu:Archive
fi

if [ $? -eq 0 ]; then
    echo ""
    echo "=================================================="
    echo "SUCCESS! Deduplication and sync completed"
    echo "=================================================="
    echo ""
    echo "Results:"
    echo "- Backup saved in: ~/maildir_backup_*"
    echo "- Batch state saved in: ~/deduplication_batch_state.json"
    echo "- Archive now contains ~30,279 unique messages"
    echo ""
    echo "Next steps:"
    echo "1. Test your email client to ensure everything works"
    echo "2. After a few days, if all is well, clean up:"
    echo "   rm -rf ~/maildir_backup_*"
    echo "   rm ~/deduplication_plan.json"
    echo "   rm ~/deduplication_batch_state.json"
else
    echo ""
    echo "Warning: Sync reported an error"
    echo "This is common with Office365 throttling"
    echo "Wait a few hours and run: $SCRIPT_DIR/sync_with_retry.sh"
    echo "The T-flag marking was successful, just need to sync to server"
fi

echo ""
echo "Current file count in Archive:"
TOTAL_FILES=$(find ~/Maildir/anu/Archive/cur -type f | wc -l)
T_FILES=$(find ~/Maildir/anu/Archive/cur -type f -name '*:2,*T*' | wc -l)
echo "  Total files: $TOTAL_FILES"
echo "  T-flagged files: $T_FILES"
echo "  Files after sync: $((TOTAL_FILES - T_FILES))"