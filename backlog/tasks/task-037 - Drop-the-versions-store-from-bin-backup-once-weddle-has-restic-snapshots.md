---
id: TASK-037
title: Drop the versions/ store from bin/backup once weddle has restic snapshots
status: To Do
assignee: []
created_date: '2026-09-20 00:29'
labels:
  - chore
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
bin/backup keeps a displaced-file store on weddle: `rclone sync --backup-dir` moves whatever a run would overwrite or delete into backup/<host>/versions/<stamp>/, and prune_versions() expires those directories after 90 days. It was added 2026-09-14 (7389a84) as a workaround for not having snapshots.

Once TASK-036 gives hourly, verified, encrypted, offsite snapshots with two-year retention, that store is a second answer to the same question, with shallower history and no verification. Removing it returns bin/backup to what it should always have been: two rclone syncs behind two guards, about 35 lines shorter.

It also removes a latent silent failure. prune_versions() derives its cutoff from `date -v-90d` falling back to `date -d`; if both fail the cutoff is empty, `[[ "$dir" < "" ]]` is false for every directory, and the prune becomes a no-op that exits 0. That path has never run --- the scheme is new enough that nothing has aged out yet.

Depends on TASK-036: landing this first leaves a window with neither versions/ nor snapshots.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 RUN_STAMP, VERSIONS, RETENTION_DAYS, prune_versions() and both --backup-dir arguments are gone from bin/backup
- [ ] #2 The comment blocks explaining displaced files and stamp-based pruning are deleted rather than annotated, per the harness-instruction rule that git holds the history
- [ ] #3 attached_image_excludes(), the metered-network guard and the weddle-is-not-a-source guard are unchanged --- a torn sparseimage copy is snapshotted just as faithfully as it was mirrored
- [ ] #4 The usage text and any CLAUDE.md or README reference to versioned copies on weddle are updated to point at the restic repo instead
- [ ] #5 bin/backup --dry-run on daysy reports the same two syncs as before
- [ ] #6 shellcheck passes on bin/backup
- [ ] #7 The existing /data/backup/daysy/versions tree is removed only after a verified restic snapshot covering it exists
<!-- AC:END -->
