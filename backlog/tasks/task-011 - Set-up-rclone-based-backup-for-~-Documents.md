---
id: task-011
title: Set up rclone-based backup for ~/Documents
status: To Do
assignee: []
created_date: "2025-10-02 01:37"
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

- [ ] #1 rclone configured with SSH/SFTP remote pointing to Linux backup server
- [ ] #2 Backup script created in bin/ directory that runs rclone sync with
      appropriate flags (progress, dry-run support)
- [ ] #3 Script integrated into dotfiles repo and symlinked via
      create_symlinks.sh if needed
- [ ] #4 Documentation added covering initial setup and usage
- [ ] #5 Optional: scheduled execution configured (launchd or cron)
<!-- AC:END -->
