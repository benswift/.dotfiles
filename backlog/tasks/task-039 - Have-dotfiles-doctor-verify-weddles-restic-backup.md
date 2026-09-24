---
id: TASK-039
title: Have dotfiles doctor verify weddle's restic backup
status: Done
assignee: []
created_date: '2026-09-24 03:30'
updated_date: '2026-09-24 05:20'
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
- [x] #1 On weddle, dotfiles doctor fails when restic-backup.timer, restic-prune.timer or restic-check.timer is not enabled
- [x] #2 On weddle, dotfiles doctor warns when the newest snapshot is older than restic-backup's staleness threshold
- [x] #3 Other hosts skip the check
<!-- AC:END -->

## Final Summary

<!-- SECTION:FINAL_SUMMARY:BEGIN -->
restic-backup gained a status command that runs its snapshot and push freshness checks, so the thresholds stay in one place. On weddle, dotfiles doctor fails on any restic-* timer that isn't enabled and warns with restic-backup's message when a snapshot or a laptop push is stale. Other hosts skip the section. Tested on weddle with a disabled timer and forced-stale thresholds, and on daysy.
<!-- SECTION:FINAL_SUMMARY:END -->
