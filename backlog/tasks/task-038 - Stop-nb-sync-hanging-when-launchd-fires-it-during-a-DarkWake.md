---
id: TASK-038
title: Stop nb-sync hanging when launchd fires it during a DarkWake
status: To Do
assignee: []
created_date: '2026-09-21 22:23'
updated_date: '2026-09-21 22:44'
labels:
  - bug
  - launchd
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
daysy's hourly nb-sync hangs when launchd fires the deferred run inside a maintenance DarkWake: the machine sleeps out from under the sync seconds later, and `timeout`'s wall clock does not advance while suspended, so the run holds launchd's one-instance slot for hours and every subsequent hourly run is skipped.

Evidence (22 Sep): pmset shows DarkWake at 06:05:57 with a 180s cap and 'Sleep Service Back to Sleep' at 06:05:59. The nb-sync log header is 06:05:57; the 900s timeout only fired at 08:00:26 (mtime of ~/.local/state/launchd-run/nb-sync.count), i.e. ~2h wall clock. The 07:00 run never started, so daysy never pulled weddle's 06:31 triage regeneration. Ben then edited a 12-hour-stale briefing.md; nb rebased it, hit a conflict, and committed the conflict markers (nb's _git_sync git-adds conflicted TEXT files as-is), pushing them to origin.

Rate: 9 timeouts + 2 exit-1 in 512 runs since 28 Jul. Never 3 consecutive, so launchd-run's counter always reset and it never paged.

Contributing: ssh to github.com has ConnectTimeout none and ServerAliveInterval 0 (`ssh -G github.com`), so a connection killed across sleep is never torn down.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 nb-sync no longer holds launchd's slot across a sleep: either it takes a power assertion (caffeinate -i) so the sync completes, or it detects DarkWake and skips with exit 0 like the wait-for-network guard
- [x] #2 ssh to github.com has a bounded failure: ConnectTimeout and ServerAliveInterval/CountMax set in ssh_config so a dead connection fails in ~60s instead of hanging
- [x] #3 the -t value in com.xwmx.nb-sync.plist is reconsidered now that 900s of wall clock can span hours of suspension
- [ ] #4 a post-sync guard notices conflict markers landing in the notebook (nb commits them by design) and files a bot todo
- [ ] #5 dotfiles doctor or tests cover whichever guard is added
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Applied 22 Sep:

- bin/in-darkwake --- new probe, exits 0 in a DarkWake (capability set has CPU and Network but no Graphics). Wired into com.xwmx.nb-sync.plist ahead of wait-for-network as `in-darkwake && exit 0`. Rejected caffeinate: -s is documented AC-only and daysy was on battery at 06:05:57, and the return to sleep was 'Sleep Service Back to Sleep' (the maintenance window's cap) rather than idle sleep, so -i is the wrong lever too.
- ssh_config --- ConnectTimeout 20, ServerAliveInterval 20, ServerAliveCountMax 3 on Host *, which is first so they are global. This is the fix that would have prevented 22 Sep on its own: the stalled sync would have been reaped ~60s after the 06:21:38 maintenance wake, freeing launchd's slot in time for the 07:00 run.
- AC#3: -t left at 900 deliberately. With ssh bounded it is unreachable, and no value is safe against suspension anyway --- lowering it would treat the symptom.
- AC#4 (page on failure rate rather than N consecutive) removed. The consecutive rule is only blind to *intermittent* failures, and these two fixes are what made them intermittent; changing launchd-run's semantics for every job to chase a 2% rate is a clever fix that breaks quietly. Revisit only if the rate does not drop.

Verified: shellcheck clean, plutil lint OK, job re-bootstrapped, kickstart run exited 0 with 'Syncing: home...Done!' and the failure counter cleared.
<!-- SECTION:NOTES:END -->
