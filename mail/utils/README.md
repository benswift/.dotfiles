# ANU Archive Maildir Deduplication Tools

This directory contains scripts to fix the ANU Archive maildir duplication issue caused by previous failed deduplication attempts.

## Problem

The Archive folder has ~92,197 files but only ~30,279 unique messages. Previous attempts to deduplicate by deleting files directly caused mbsync to try re-downloading them from the server. The sync state is corrupted with a 39MB journal file.

## Solution

These scripts implement the proper T-flag deletion approach:
1. Mark duplicate files with the T (Trash) flag
2. Let mbsync sync these flags to the server (deleting on server)
3. Remove T-flagged files locally after successful sync

## Scripts

### deduplicate_maildir.py
Main Python script that:
- Analyzes maildir for duplicates (by Message-ID)
- Creates a deduplication plan
- Marks files with T flag in batches
- Handles Office365 throttling with batch processing
- Maintains state for resumable operations
- Checks sync state health before proceeding

**Key features:**
- `--dry-run`: Analyze and create plan without changes
- `--execute --batch`: Process in safe batches
- `--check-health`: Verify sync state before starting
- `--restore`: Restore from backup if needed

### fix_anu_archive_duplicates.sh
Complete workflow script that:
- Checks sync state health
- Runs dry-run analysis
- Offers batch or all-at-once processing
- Creates backups
- Verifies results
- Initiates server sync

### sync_with_retry.sh
Robust mbsync wrapper that:
- Handles Office365 throttling with exponential backoff
- Detects and handles OAuth token expiry
- Provides detailed progress reporting
- Retries up to 20 times with intelligent backoff
- Detects sync state corruption

### process_batch.sh
Simple batch processor that:
- Processes one batch of files at a time
- Shows progress between batches
- Allows easy stop/resume

## Usage

### Quick Start (Recommended)

```bash
# Run the main fix script
cd ~/.dotfiles/mail/utils
./fix_anu_archive_duplicates.sh

# Choose option 1 (batch mode) when prompted
```

### Manual Step-by-Step

```bash
# 1. Check sync state health
python3 deduplicate_maildir.py --check-health

# If unhealthy, fix the journal:
mv ~/Maildir/anu/Archive/.mbsyncstate.journal ~/backup/
mbsync --pull anu:Archive

# 2. Analyze and create plan
python3 deduplicate_maildir.py --dry-run

# 3. Process in batches (safer)
./process_batch.sh 1000  # Process 1000 files
# Repeat until all batches done

# 4. Sync with server
./sync_with_retry.sh

# 5. Clean up T-flagged files locally
find ~/Maildir/anu/Archive/cur -name '*:2,*T*' -delete
```

## Critical Rules for Maildir Deduplication

1. **NEVER delete files directly** - Always mark with T flag first:
   ```bash
   # Correct: Mark with T flag
   mv "file:2,S" "file:2,ST"
   
   # Wrong: Delete directly
   rm "file:2,S"
   ```

2. **NEVER clear .mbsyncstate files** - This causes re-duplication

3. **Batch operations for Office365** - Process 500-1000 files at a time

4. **Handle OAuth expiry** - Tokens expire during long operations

5. **Respect throttling** - Use exponential backoff

## Office365 Specific Issues

- Aggressive throttling after ~1000 operations
- OAuth tokens expire during long syncs
- PipelineDepth must be 1 (no pipelining)
- Provides backoff times in error messages

## Recovery

If something goes wrong:

```bash
# Restore from backup
python3 deduplicate_maildir.py --restore

# Or manually
rm -rf ~/Maildir/anu/Archive
mv ~/maildir_backup_* ~/Maildir/anu/Archive
```

## Expected Results

- Before: ~92,197 files (with ~60,000 duplicates)
- After: ~30,279 unique messages
- Server: Duplicates deleted via T-flag sync
- Local: Clean maildir with no duplicates

See task-010 in backlog for the complete implementation plan.