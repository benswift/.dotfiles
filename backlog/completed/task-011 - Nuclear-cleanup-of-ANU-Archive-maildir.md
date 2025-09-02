---
id: task-011
title: Nuclear cleanup of ANU Archive maildir
status: Done
assignee: []
created_date: "2025-08-29 10:42"
labels: ["email", "critical", "data-recovery"]
dependencies: ["task-010"]
---

## Description

Complete cleanup and resync of Archive folder to fix duplication and sync
corruption issues following the partial sync failure from task-010. Use the
"nuclear option" to empty server Archive and re-upload clean messages.

## Current State (as of 2025-08-29 23:40)

**Local maildir:**

- Total files: 91,120 (restored from backup)
- Unique messages: ~30,281 (needs re-analysis)
- Duplicates: ~60,839 (estimated)
- Message date range: January 2021 - October 2024
- Previous deduplication plan may need updating

**Server (Office365):**

- Archive folder: 0 messages (emptied manually via Outlook)
- Archive-OLD folder contains the corrupted backup
- Ready for clean re-upload

**Problems:**

- mbsync state corruption (63MB journal file)
- Massive re-duplication during sync
- Cannot use bidirectional sync without fixing

## Solution: Nuclear Option

Empty the server Archive folder and re-upload only the 30,281 unique messages.

## Implementation Steps

### 1. Local Cleanup

Run the deduplication analysis:

```bash
cd ~/.dotfiles/mail/utils
python3 deduplicate_maildir.py --dry-run
```

Extract unique messages (creates backup, keeps only unique):

```bash
python3 extract_unique_messages.py
```

This will:

- Create backup at `~/Maildir/anu/Archive-with-dupes-TIMESTAMP/`
- Keep only 30,281 unique messages in `~/Maildir/anu/Archive/`
- Remove corrupted mbsync state files

### 2. Fix File Timestamps ⚠️ CRITICAL

**DISCOVERED ISSUE**: mbsync uses the file's filesystem timestamp (not the
email's Date header) as the IMAP INTERNALDATE when uploading. Without fixing
timestamps first, all emails will show today's date in Outlook.

After deduplication, fix timestamps on all remaining messages:

```bash
cd ~/.dotfiles/mail/utils
python3 fix_maildir_timestamps.py ~/Maildir/anu/Archive/cur
```

This will update each file's timestamp to match its Date header, ensuring
correct dates after upload.

### 3. Server Preparation ✓ COMPLETE

In Outlook (Web or Mac):

1. ✓ Renamed "Archive" folder to "Archive-OLD"
2. ✓ Created new empty "Archive" folder
3. ✓ Verified mbsync sees 0 messages in Archive

### 4. Execute Nuclear Cleanup

Run the complete cleanup script:

```bash
cd ~/.dotfiles/mail/utils
./nuclear_archive_cleanup.sh
```

This script will:

1. Extract unique messages locally (if not already done)
2. Verify server Archive was renamed
3. Sync empty Archive from server
4. Upload 30,281 clean messages to server
5. Verify final state

### 5. Post-Cleanup

After successful completion:

1. Verify in Outlook that Archive has ~30,281 messages
2. Test bidirectional sync works correctly
3. After a few days of stable operation:
   - Delete "Archive-OLD" folder on server
   - Delete local backup: `rm -rf ~/Maildir/anu/Archive-with-dupes-*`
   - Clean up: `rm ~/deduplication_plan.json ~/deduplication_batch_state.json`

## Scripts Created

All scripts are in `~/.dotfiles/mail/utils/`:

- `deduplicate_maildir.py` - Analyzes and creates deduplication plan
- `extract_unique_messages.py` - Extracts unique messages based on dedup plan
- `fix_maildir_timestamps.py` - Fixes file timestamps to match Date headers
  (CRITICAL for date preservation)
- `nuclear_archive_cleanup.sh` - Orchestrates the complete nuclear cleanup
- Test scripts (can be removed): `test_date_preservation*.sh`

## Success Criteria

- Local Archive: exactly 30,281 unique messages
- Server Archive: exactly 30,281 messages
- Bidirectional sync works without re-downloading
- No sync state corruption
- Message dates preserved (Jan 2021 - Oct 2024)

## Rollback Plan

If anything goes wrong:

1. Local backup at: `~/Maildir/anu/Archive-with-dupes-*/`
2. Server backup: "Archive-OLD" folder
3. Can restore both and try alternative approaches

## Lessons Learned

- Never run mbsync during partial T-flag state
- Office365 corruption can cause re-downloads
- Nuclear option is cleaner than fighting sync corruption
- Always backup before major operations
- **CRITICAL**: mbsync uses filesystem timestamps (not Date headers) for IMAP
  INTERNALDATE
- Must fix file timestamps before uploading to preserve email dates in
  Outlook/Office365
