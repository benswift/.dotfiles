---
id: task-010
title: Fix ANU Archive maildir duplication issue
status: To Do
assignee: []
created_date: '2025-08-28 12:05'
labels: []
dependencies: []
---

## Description

The ANU Archive maildir contains 91,120 files but only 30,279 unique messages. Need to safely remove 60,841 duplicate files while preserving all unique messages and maintaining maildir integrity.

## Problem

The recent deduplication attempt on ~/Maildir/anu/Archive was only partially successful:
- **Current state**: 91,120 files containing only 30,279 unique messages
- **Duplication level**: 67% of files are duplicates (60,841 unnecessary files)
- **Severity**: Some messages have up to 13 copies
- **Root cause**: Previous deduplication script failed to complete properly

This is causing:
- Excessive disk usage (3x more files than needed)
- Slow email client performance
- Potential sync issues with mbsync

## Approach

Create a single, robust Python script (`deduplicate_maildir.py`) that:

1. **Full backup first** (critical):
   - Create complete backup of Archive folder with actual messages (not just filelist)
   - Verify backup integrity before proceeding

2. **Analyse and identify duplicates**:
   - Use Message-ID as primary deduplication key
   - Fall back to content hash for messages without Message-ID
   - Keep the oldest file (by timestamp in filename) for each unique message
   - Preserve all maildir flags (S, R, T, etc.)

3. **Safe deletion process**:
   - Generate deletion plan and review before executing
   - Move duplicates to quarantine folder first (not immediate deletion)
   - Verify message count remains at 30,279 unique messages
   - Only permanently delete after verification

4. **Sync with server**:
   - Run mbsync with appropriate flags to propagate deletions
   - Verify server state matches local

## Implementation steps

1. **Clean up existing scripts**:
   ```bash
   rm -f ~/check_maildir_sanity.py
   rm -f ~/check_archive_duplicates.py
   rm -f ~/analyze_backup.py
   rm -f ~/final_sanity_check.py
   rm -f ~/dedupe_and_sync.sh
   rm -f ~/safe_dedupe_and_sync.sh
   ```

2. **Create comprehensive deduplication script**:
   - Single Python script with clear phases
   - Extensive logging and progress reporting
   - Dry-run mode for testing
   - Rollback capability if issues detected

3. **Execute deduplication**:
   ```bash
   # Dry run first
   python3 deduplicate_maildir.py --dry-run
   
   # Review plan, then execute
   python3 deduplicate_maildir.py --execute
   
   # Verify results
   python3 deduplicate_maildir.py --verify
   ```

4. **Post-deduplication**:
   - Clear mbsync state files if needed
   - Run full sync to ensure server consistency
   - Monitor for any issues over next few days

## Success criteria

- Archive folder contains exactly 30,279 files (one per unique message)
- No duplicate Message-IDs exist
- All original unique messages preserved
- Successful sync with remote server
- Email client performance improved

## Cleanup notes

After successful completion:
- Remove quarantine folder
- Delete backup if everything verified working
- Remove deduplication script from working directory
- Document final state in mail/README.md if needed
