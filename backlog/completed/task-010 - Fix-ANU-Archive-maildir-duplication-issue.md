---
id: task-010
title: Fix ANU Archive maildir duplication issue
status: Done
assignee: []
created_date: "2025-08-28 12:05"
updated_date: "2025-08-29 05:08"
labels: []
dependencies: []
---

THIS IS MORE OF A WONTFIX, REALLY.

## Description

Fix the broken ANU Archive maildir state caused by previous deduplication
attempts. The maildir now has corrupted sync state with mbsync trying to
re-download thousands of "deleted" duplicates from the server.

## Current broken state

**File counts (as of 2025-08-29)**:

- Total files in Archive: 92,197 (increased from failed recovery attempts)
- Unique messages: ~30,279 (unchanged)
- Duplicate files: ~60,000+
- Files with T flag (marked for deletion): Only 1 (should be ~60,000)
- mbsync trying to re-download: 28,210 "deleted" duplicates from server

**Sync state corruption**:

- mbsync journal file: 39MB (indicates corrupted state)
- Previous deduplication deleted files locally without marking T flag first
- Server still has the messages, so mbsync wants to re-download them
- Office365 throttling prevents bulk operations

## Root cause analysis

**Why previous attempts failed**:

1. **Direct file deletion**: Deleted duplicate files without marking T flag
   first
2. **Sync state cleared**: Removed `.mbsyncstate` files causing re-duplication
3. **Office365 throttling**: Bulk operations hit aggressive rate limits
4. **OAuth token expiry**: Long operations fail when tokens expire mid-process
5. **No batching**: Attempted to process all files at once

**Key learnings**:

- NEVER delete maildir files directly - must mark with T flag first
- NEVER clear mbsync state files - causes re-duplication from server
- Office365 requires careful throttling and backoff strategies
- OAuth tokens expire during long operations and need refresh handling

## Implementation Notes

Updated with current broken state, root cause analysis, and proper T-flag based
solution approach

## Correct approach

Must use proper maildir T-flag deletion process with batched operations:

1. **T-flag marking phase** (batchable overnight runs):

   - Identify duplicates (same Message-ID, keep oldest file)
   - Mark duplicates with T flag (rename files to add :2,T suffix)
   - Process in batches of 500-1000 files per run
   - Handle OAuth token refresh gracefully
   - Respect Office365 throttling with exponential backoff

2. **Sync and verify phase**:

   - Run `mbsync anu:Archive` to sync T-flagged deletions to server
   - Verify server deletions completed successfully
   - Handle throttling and retry failed deletions

3. **Final cleanup phase**:
   - Remove T-flagged files from local maildir
   - Verify final counts match expectations
   - No sync state corruption

## Implementation plan

1. **Create batched T-flag marking script**:

   ```python
   # Process duplicates in batches
   # Mark with T flag (proper maildir deletion)
   # Handle Office365 throttling
   # Save progress state for resumability
   # OAuth token refresh handling
   ```

2. **Batch execution strategy**:

   ```bash
   # Run in batches over multiple nights
   python3 mark_duplicates_batch.py --batch-size 1000 --start-batch 0
   python3 mark_duplicates_batch.py --batch-size 1000 --start-batch 1
   # ... continue until all duplicates marked
   ```

3. **Sync and cleanup**:

   ```bash
   # Sync T-flagged deletions to server
   mbsync anu:Archive

   # Remove T-flagged files locally
   python3 cleanup_t_flagged.py

   # Verify final state
   python3 verify_deduplication.py
   ```

## Safety requirements

**Must be batchable**:

- Process 500-1000 files per batch maximum
- Save progress state to resume after failures
- Handle OAuth token expiry gracefully
- Respect Office365 rate limits with backoff

**Must handle new mail safely**:

- User won't archive new mail during process
- Only process existing duplicates
- Preserve new incoming messages

**Must maintain sync integrity**:

- Never clear sync state files
- Use proper T-flag deletion process
- Verify server sync after each batch
- Handle partial failures gracefully

## Success criteria

- All ~60,000 duplicate files marked with T flag
- Server successfully synced deletions (reduced message count)
- Local Archive contains exactly ~30,279 unique messages
- No sync state corruption
- mbsync operates normally without re-downloading
- Process completed in batches without breaking email workflow

## Verification steps

1. **After T-flag marking**:

   ```bash
   # Count T-flagged files
   find ~/Maildir/anu/Archive -name "*:2,*T*" | wc -l
   # Should show ~60,000 files
   ```

2. **After server sync**:

   ```bash
   # Verify server message count decreased
   # Check mbsync no longer tries to re-download
   ```

3. **After final cleanup**:

   ```bash
   # Total files should be ~30,279
   find ~/Maildir/anu/Archive -type f | wc -l

   # No T-flagged files should remain
   find ~/Maildir/anu/Archive -name "*:2,*T*" | wc -l
   # Should show 0
   ```
