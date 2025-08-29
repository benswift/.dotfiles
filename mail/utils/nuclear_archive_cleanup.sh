#!/bin/bash
# Nuclear cleanup: Empty server Archive, upload clean local messages
# This script coordinates the full nuclear cleanup process

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=================================================="
echo "Nuclear Archive Cleanup"
echo "=================================================="
echo ""
echo "This script will:"
echo "1. Extract unique messages locally (backup duplicates)"
echo "2. Verify server Archive has been renamed"
echo "3. Sync empty Archive from server"  
echo "4. Upload clean messages to server"
echo "5. Verify final state"
echo ""

# Step 1: Extract unique messages locally
echo "Step 1: Extracting unique messages locally..."
echo ""

if [ ! -f ~/deduplication_plan.json ]; then
    echo "ERROR: No deduplication plan found"
    echo "Run: python3 $SCRIPT_DIR/deduplicate_maildir.py --dry-run"
    exit 1
fi

echo "Current state:"
TOTAL=$(find ~/Maildir/anu/Archive/cur -type f 2>/dev/null | wc -l)
echo "  Total files in Archive: $TOTAL"

if [ "$TOTAL" -eq 0 ]; then
    echo "ERROR: Archive is empty. Nothing to process."
    exit 1
fi

# Extract unique messages
python3 "$SCRIPT_DIR/extract_unique_messages.py"

if [ $? -ne 0 ]; then
    echo "ERROR: Failed to extract unique messages"
    exit 1
fi

echo ""
echo "Local cleanup complete!"
CLEAN_COUNT=$(find ~/Maildir/anu/Archive/cur -type f | wc -l)
echo "Archive now has $CLEAN_COUNT unique messages"

# Step 2: Check server state
echo ""
echo "=================================================="
echo "Step 2: Server preparation"
echo "=================================================="
echo ""
echo "IMPORTANT: Before continuing, you must:"
echo "1. Open Outlook (Web or Mac)"
echo "2. Rename 'Archive' folder to 'Archive-OLD' (or similar)"
echo "3. Create a new empty 'Archive' folder"
echo ""
read -p "Have you completed these steps? (yes/no): " server_confirm

if [ "$server_confirm" != "yes" ]; then
    echo "Please complete server steps first, then re-run this script"
    exit 0
fi

# Step 3: Sync empty Archive from server
echo ""
echo "Step 3: Syncing empty Archive from server..."
echo "This will temporarily clear local Archive (we have backup)"
echo ""

# Backup current clean state just in case
cp -r ~/Maildir/anu/Archive ~/Maildir/anu/Archive-clean-backup

# Try to sync (may fail initially if folder was just created)
mbsync anu:Archive 2>&1 | tee /tmp/mbsync_output.txt

# Check if it worked
if grep -q "error\|Error\|ERROR" /tmp/mbsync_output.txt; then
    echo "Note: Initial sync had errors (expected if folder is new)"
fi

# Count files - should be 0 or very few
SERVER_COUNT=$(find ~/Maildir/anu/Archive/cur -type f | wc -l)
echo "After sync from server: $SERVER_COUNT files"

if [ "$SERVER_COUNT" -gt 100 ]; then
    echo "ERROR: Server Archive doesn't appear to be empty!"
    echo "Found $SERVER_COUNT files. Expected < 100"
    echo "Did you rename the old Archive folder on the server?"
    
    # Restore clean backup
    rm -rf ~/Maildir/anu/Archive
    mv ~/Maildir/anu/Archive-clean-backup ~/Maildir/anu/Archive
    exit 1
fi

# Step 4: Copy clean messages back and upload
echo ""
echo "Step 4: Uploading $CLEAN_COUNT clean messages to server..."
echo ""

# If server was empty, copy our clean messages back
if [ "$SERVER_COUNT" -lt "$CLEAN_COUNT" ]; then
    rm -rf ~/Maildir/anu/Archive/cur/*
    cp -r ~/Maildir/anu/Archive-clean-backup/cur/* ~/Maildir/anu/Archive/cur/
fi

# Remove backup
rm -rf ~/Maildir/anu/Archive-clean-backup

echo "Starting upload sync (this will take a while)..."
echo "Office365 may throttle - this is normal"
echo ""

# Use the retry script if available
if [ -f "$SCRIPT_DIR/sync_with_retry.sh" ]; then
    bash "$SCRIPT_DIR/sync_with_retry.sh"
else
    mbsync anu:Archive
fi

# Step 5: Final verification
echo ""
echo "=================================================="
echo "Step 5: Final verification"
echo "=================================================="
echo ""

FINAL_LOCAL=$(find ~/Maildir/anu/Archive/cur -type f | wc -l)
echo "Final local count: $FINAL_LOCAL messages"
echo ""
echo "Expected: ~30,281 unique messages"
echo ""

if [ "$FINAL_LOCAL" -lt 30000 ] || [ "$FINAL_LOCAL" -gt 31000 ]; then
    echo "WARNING: Final count seems off. Check manually."
else
    echo "SUCCESS! Archive has been cleaned"
fi

echo ""
echo "Next steps:"
echo "1. Verify in Outlook that Archive has ~30k messages"
echo "2. After a few days, delete 'Archive-OLD' folder on server"
echo "3. Delete local backup: rm -rf ~/Maildir/anu/Archive-with-dupes-*"
echo "4. Clean up: rm ~/deduplication_plan.json ~/deduplication_batch_state.json"
echo ""
echo "Archive date range: January 2021 - October 2024"