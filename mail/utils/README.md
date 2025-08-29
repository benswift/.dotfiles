# Mail utilities

This folder contains scripts used for debugging and fixing email synchronisation issues, particularly the ANU Archive duplication problem.

## Scripts

### fix_anu_archive_duplicates.sh
Main script to orchestrate the deduplication process.
**Fixed**: No longer clears mbsync state files, uses correct script paths.

### sync_with_retry.sh  
Retry wrapper for mbsync with proper throttling support.
**Fixed**: Now extracts and respects Office365 throttling backoff times, handles OAuth expiry.

### deduplicate_maildir.py
Python script to identify and mark duplicates with T flags.
**Fixed**: Now properly renames files to add T flag instead of moving/deleting them.

## Lessons learned

### Critical rules for maildir deduplication:

1. **NEVER delete files directly** - Always mark with T (Trash) flag first, then sync
2. **NEVER clear .mbsyncstate files** - This causes mbsync to lose track and re-download everything
3. **Always use T flags** - mbsync needs these to know what to delete on server:
   ```bash
   # Correct: Mark with T flag
   mv "file:2,S" "file:2,ST"
   
   # Wrong: Delete directly
   rm "file:2,S"
   ```

4. **Batch operations for Office365** - Process in chunks of 500-1000 to avoid throttling
5. **Handle OAuth expiry** - Refresh tokens before long operations
6. **Respect throttling** - Use exponential backoff, not fixed delays

### Office365 specific issues:

- Aggressive throttling after ~1000 operations
- OAuth tokens expire during long syncs
- PipelineDepth must be 1 (no pipelining)
- Suggests backoff times in error messages - respect them!

### Current state (as of 2025-08-29):

- 92,197 files in Archive folder
- Only ~30,279 unique messages
- 60,000+ duplicates still present
- mbsync sync state corrupted - wants to re-download deleted files
- Need proper T-flag based deduplication approach

See task-010 in backlog for the complete fix plan.