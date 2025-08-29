#!/bin/bash
# Batch processing wrapper for T-flag marking
# Processes files in safe batches to avoid Office365 throttling

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BATCH_SIZE=${1:-1000}  # Default 1000 files per batch

echo "=================================================="
echo "Batch T-flag Processing"
echo "=================================================="
echo ""
echo "This script will mark duplicate files with T flag in batches"
echo "Batch size: $BATCH_SIZE files"
echo ""

# Check if plan exists
if [ ! -f ~/deduplication_plan.json ]; then
    echo "ERROR: No deduplication plan found"
    echo "Run this first: python3 $SCRIPT_DIR/deduplicate_maildir.py --dry-run"
    exit 1
fi

# Get total files to process from plan
TOTAL_TO_DELETE=$(python3 -c "import json; print(len(json.load(open('$HOME/deduplication_plan.json'))['files_to_delete']))")
echo "Total files to mark: $TOTAL_TO_DELETE"

# Check batch state
if [ -f ~/deduplication_batch_state.json ]; then
    PROCESSED=$(python3 -c "import json; print(json.load(open('$HOME/deduplication_batch_state.json')).get('processed_index', 0))")
    REMAINING=$(python3 -c "import json; print(json.load(open('$HOME/deduplication_batch_state.json')).get('remaining', $TOTAL_TO_DELETE))")
    echo "Already processed: $PROCESSED"
    echo "Remaining: $REMAINING"
else
    PROCESSED=0
    REMAINING=$TOTAL_TO_DELETE
fi

if [ "$REMAINING" -eq 0 ]; then
    echo ""
    echo "All files already processed!"
    echo "Run sync to delete on server: $SCRIPT_DIR/sync_with_retry.sh"
    exit 0
fi

echo ""
echo "Processing next batch of $BATCH_SIZE files..."
echo ""

# Process one batch
python3 "$SCRIPT_DIR/deduplicate_maildir.py" --execute --batch --batch-size "$BATCH_SIZE" --resume --no-backup

if [ $? -eq 0 ]; then
    # Check new state
    NEW_REMAINING=$(python3 -c "import json; print(json.load(open('$HOME/deduplication_batch_state.json')).get('remaining', 0))")
    
    echo ""
    echo "Batch complete!"
    echo "Files remaining: $NEW_REMAINING"
    
    if [ "$NEW_REMAINING" -eq 0 ]; then
        echo ""
        echo "=================================================="
        echo "All batches complete!"
        echo "=================================================="
        echo ""
        echo "Next steps:"
        echo "1. Verify the T-flag marking:"
        echo "   find ~/Maildir/anu/Archive/cur -name '*:2,*T*' | wc -l"
        echo ""
        echo "2. Sync with server (this will take time):"
        echo "   $SCRIPT_DIR/sync_with_retry.sh"
        echo ""
        echo "3. After successful sync, remove T-flagged files locally:"
        echo "   find ~/Maildir/anu/Archive/cur -name '*:2,*T*' -delete"
    else
        echo ""
        echo "To continue processing:"
        echo "  $0 $BATCH_SIZE"
        echo ""
        echo "Or to process all remaining at once:"
        echo "  $0 $NEW_REMAINING"
    fi
else
    echo ""
    echo "ERROR: Batch processing failed"
    echo "Check the logs at: ~/deduplicate_maildir_*.log"
    exit 1
fi