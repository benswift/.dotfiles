---
id: TASK-039
title: Have dotfiles doctor verify weddle's restic backup
status: To Do
assignee: []
created_date: '2026-09-24 03:30'
labels:
  - enhancement
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Carried over from TASK-036 #10. The new symlinks are already in lib/symlink-manifest.sh, so doctor checks those. It does not yet check the rest of the setup on weddle: that the three restic-* timers are enabled and that the newest snapshot is recent.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 On weddle, dotfiles doctor fails when restic-backup.timer, restic-prune.timer or restic-check.timer is not enabled
- [ ] #2 On weddle, dotfiles doctor warns when the newest snapshot is older than restic-backup's staleness threshold
- [ ] #3 Other hosts skip the check
<!-- AC:END -->
