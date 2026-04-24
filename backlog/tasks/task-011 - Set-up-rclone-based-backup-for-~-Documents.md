---
id: TASK-011
title: Set up rclone-based backup for ~/Documents
status: Done
assignee: []
created_date: '2025-10-02 01:37'
updated_date: '2026-04-24 01:59'
labels:
  - backup
  - automation
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Configure rclone to sync ~/Documents from this macOS machine to remote Linux
machine. This replaces manual backups with automated sync solution.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 rclone configured with SSH/SFTP remote pointing to Linux backup server
- [x] #2 Backup script created in bin/ directory that runs rclone sync with
      appropriate flags (progress, dry-run support)
- [x] #3 Script integrated into dotfiles repo and symlinked via
      create_symlinks.sh if needed
- [x] #4 Documentation added covering initial setup and usage
- [x] #5 Optional: scheduled execution configured (launchd or cron)
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Implemented as bin/rclone-backup.sh (syncs ~/Documents and ~/Maildir to weddle:backup/mitch/). Scripts in bin/ are on PATH via zshenv. AC #5 (scheduled execution) intentionally not set up --- user prefers manual invocation.
<!-- SECTION:NOTES:END -->
