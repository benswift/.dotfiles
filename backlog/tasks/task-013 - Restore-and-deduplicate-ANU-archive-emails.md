---
id: task-013
title: Restore and deduplicate ANU archive emails
status: To Do
assignee: []
created_date: "2025-09-01 14:08"
labels: []
dependencies: []
---

Copy the most complete ANU archive backup and remove duplicates, keeping only
unique messages for local mu searching without mbsync

## Background

Analysis of ANU archive backups revealed:

- Current ~/Maildir/anu/Archive only has 32 messages
- Backup at mail/backups-and-logs/anu-20250830-pre-resync/Archive contains
  30,279 unique messages (with 60,841 duplicates)
- These 30,279 messages are not in the current maildir

## Objective

Create a local-only archive at ~/Maildir/anu-archive that:

- Contains all 30,279 unique messages from the backup
- Has no duplicates
- Is searchable via mu
- Is NOT touched by mbsync (no server sync)

## Implementation steps

### 1. Copy the archive backup

```bash
cp -r mail/backups-and-logs/anu-20250830-pre-resync/Archive ~/Maildir/anu-archive
```

This will copy 91,120 files (30,279 unique + 60,841 duplicates)

### 2. Create deduplication script

Write a Python script `mail/deduplicate_maildir.py` that:

- Scans the maildir at ~/Maildir/anu-archive
- Identifies duplicates using Message-ID headers (all messages have them)
- Deletes duplicate files, keeping only one copy of each unique message
- Provides progress updates and summary statistics

Key requirements for the script:

- Use the same Message-ID extraction logic from the analysis script
- Keep the first occurrence of each message (by filename sort)
- Delete subsequent duplicates
- Preserve maildir structure (cur/, new/, tmp/)
- Report: files processed, unique messages kept, duplicates removed

### 3. Run deduplication

```bash
python3 mail/deduplicate_maildir.py ~/Maildir/anu-archive
```

Expected result: 30,279 unique messages remain, 60,841 duplicates removed

### 4. Update mu index

```bash
mu index --maildir=~/Maildir
```

This will include the new anu-archive folder in searches

### 5. Verify mbsync configuration

Ensure mbsyncrc does NOT reference ~/Maildir/anu-archive anywhere. It should
only sync:

- ~/Maildir/anu/INBOX
- ~/Maildir/anu/Archive (the current one with 32 messages)
- ~/Maildir/anu/"Sent Items"
- etc.

The anu-archive folder will be local-only.

## Success criteria

- [ ] ~/Maildir/anu-archive exists with exactly 30,279 unique email files
- [ ] No duplicate messages remain in anu-archive
- [ ] mu search includes messages from anu-archive
- [ ] mbsync does not touch anu-archive during sync operations
- [ ] Original backup in mail/backups-and-logs remains unchanged

## Notes

- The deduplication script should be reusable for other maildir folders
- Consider adding a --dry-run option to preview what would be deleted
- Keep the script in the mail/ directory for future use
