---
id: TASK-036
title: Add an offsite restic/B2 backup of weddle aggregated data
status: In Progress
assignee: []
created_date: '2026-09-20 00:28'
updated_date: '2026-09-23 04:34'
labels:
  - enhancement
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
weddle is the single aggregation point for every host backup, and nothing backs up weddle.

- /data/backup/daysy/desktop-archive (252 GB) exists nowhere else
- /data/backup/daysy/Documents (69 GB) and /data/Maildir (39 GB) have a second copy only on daysy, in the same building
- ~/claude-logs (24 GB) is the only place the union of every host session log exists

That is 2 copies, 1 site, 0 offsite: a fire, a theft, or a deletion that the hourly mirror propagates takes both copies.

A restic repository on Backblaze B2 adds the offsite copy. restic rather than plain `rclone sync` because it gives client-side encryption (which is what makes sending Maildir and Documents/admin to a third party acceptable), content-addressed dedup, real snapshots, and `restic check` as actual verification. B2 egress is free up to 3x stored data, so a full restore costs nothing.

restic is in the mise registry as aqua:restic/restic, with prebuilt linux-amd64 and darwin-arm64 binaries.

Sizing: the tier-1 set is roughly 385 GB, about USD 2.30/month at B2 rates. The initial upload is the slow part --- 2 days at 20 Mbit up, ~17 hours at 50 Mbit --- so it wants a deliberate kickoff.

This snapshots a mirror, so the chain is delete-on-daysy, mirror, snapshot: retention depth is the protection, not the snapshot being taken at source. Running restic at source on each host into the same repo is the purer design and can be added later without redoing this, but daysy is a laptop on sometimes-metered links and desktop-archive only exists on weddle, so weddle needs its own job either way.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 restic is pinned in mise/config.toml and resolves on both weddle and daysy
- [x] #2 A B2 bucket and a bucket-scoped application key exist; the key and the repo password live only in the untracked mise [env] block, never in a tracked file
- [x] #3 The repo password is recorded somewhere off weddle, so losing weddle does not lose the archive
- [ ] #4 bin/restic-backup snapshots the tier-1 set (/data/backup, /data/Maildir, ~/claude-logs, ~/codex-logs, ~/.nb) and excludes the regenerable trees (/data/huggingface, /data/uv-cache, /data/panic_tda, ~/projects/panic_tda)
- [ ] #5 A systemd user timer runs it hourly on weddle, staggered clear of push-to-weddle, with OnFailure=unit-oncall@%n.service
- [ ] #6 Retention is one declarative forget policy (--keep-hourly 48 --keep-daily 14 --keep-weekly 8 --keep-monthly 24), with prune on a monthly schedule rather than every run
- [ ] #7 The job exits non-zero when the newest snapshot is older than a staleness threshold, so a run that succeeds and stores nothing pages --- the same guard as ingest-claude-logs
- [ ] #8 restic check --read-data-subset runs on a schedule and pages on failure
- [ ] #9 A test restore of a known file from the B2 repo succeeds using only the recorded password and credentials
- [ ] #10 Every new symlink target is added to lib/symlink-manifest.sh, and dotfiles doctor verifies the setup
<!-- AC:END -->
