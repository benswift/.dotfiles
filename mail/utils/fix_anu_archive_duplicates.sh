#!/bin/bash
# Complete script to fix ANU Archive maildir duplication issue
# This script will deduplicate the Archive folder and sync with the server

set -e  # Exit on error

echo "=================================================="
echo "ANU Archive Maildir Deduplication Script"
echo "=================================================="
echo ""
echo "This script will:"
echo "1. Run deduplication in dry-run mode"
echo "2. Create a full backup"
echo "3. Mark 60,841 duplicate files with T flag (keeping 30,279 unique messages)"
echo "4. Sync changes with the server"
echo ""

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check if deduplicate script exists
if [ ! -f "$SCRIPT_DIR/deduplicate_maildir.py" ]; then
    echo "Error: $SCRIPT_DIR/deduplicate_maildir.py not found"
    exit 1
fi

# Step 1: Run dry-run (if plan doesn't exist)
if [ ! -f ~/deduplication_plan.json ]; then
    echo "Step 1: Running dry-run analysis..."
    python3 "$SCRIPT_DIR/deduplicate_maildir.py" --dry-run
    
    if [ $? -ne 0 ]; then
        echo "Error: Dry-run failed"
        exit 1
    fi
else
    echo "Step 1: Using existing deduplication plan"
fi

echo ""
echo "Dry-run complete. Review the plan above."
echo "Expected result: 91,120 files -> 30,279 files"
echo ""
read -p "Continue with deduplication? (yes/no): " confirm

if [ "$confirm" != "yes" ]; then
    echo "Aborted by user"
    exit 0
fi

# Step 2: Execute deduplication
echo ""
echo "Step 2: Executing deduplication (this will take several minutes)..."
python3 "$SCRIPT_DIR/deduplicate_maildir.py" --execute

if [ $? -ne 0 ]; then
    echo "Error: Deduplication failed"
    echo "You may need to restore from backup"
    exit 1
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

# Step 4: Sync with server (DO NOT clear mbsync state!)
echo ""
echo "Step 4: Syncing with server (this may take a while)..."
echo "This will EXPUNGE deleted messages on the server (due to 'Expunge Both' in mbsyncrc)"
echo ""
read -p "Proceed with server sync and expunge? (yes/no): " sync_confirm
if [ "$sync_confirm" != "yes" ]; then
    echo "Server sync skipped. Local deduplication is complete."
    echo "To sync later, run: mbsync anu:Archive"
    exit 0
fi

echo "Running: mbsync anu:Archive"
mbsync anu:Archive

if [ $? -eq 0 ]; then
    echo ""
    echo "=================================================="
    echo "SUCCESS! Deduplication and sync completed"
    echo "=================================================="
    echo ""
    echo "Results:"
    echo "- Backup saved in: ~/maildir_backup_*"
    echo "- Quarantined files in: ~/maildir_quarantine_*"
    echo "- Archive now contains 30,279 unique messages"
    echo ""
    echo "Next steps:"
    echo "1. Test your email client to ensure everything works"
    echo "2. After a few days, if all is well:"
    echo "   rm -rf ~/maildir_quarantine_*"
    echo "   rm -rf ~/maildir_backup_*"
    echo "   rm ~/deduplication_plan.json"
else
    echo ""
    echo "Warning: mbsync reported an error"
    echo "Check the sync status and logs"
    echo "The local deduplication was successful, but server sync may need attention"
fi

echo ""
echo "Current file count in Archive:"
find ~/Maildir/anu/Archive/cur -type f | wc -l