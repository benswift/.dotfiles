---
id: task-013
title:
  Investigate fix for ANU Exchange calendar sync stopping after days of uptime
status: To Do
assignee: []
created_date: "2025-10-19 21:28"
updated_date: "2025-10-19 21:28"
labels:
  - macos
  - calendar
  - oauth2
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

The macOS Calendar.app sync with ANU Office365 Exchange account stops working
after the machine has been running for several days. Restarting the machine
causes a flurry of old iCal notifications to appear, indicating the sync had
been stuck. This affects the u2548636@anu.edu.au account which uses OAuth2
authentication (same setup as configured in mbsyncrc/msmtprc).

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [ ] #1 Understand why OAuth2 token refresh fails during network transitions
- [ ] #2 Identify why long-running calendar daemons accumulate failed connection
      state
- [ ] #3 Determine correlation between network state changes and sync failures
- [ ] #4 Implement a reliable fix that doesn't require periodic restarts
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

## Investigation findings

### Account details

- Exchange account: u2548636@anu.edu.au
- Uses OAuth2 authentication (same setup as mail in mbsyncrc/msmtprc)
- Account shows as active and authenticated in Calendar.sqlitedb (ZACTIVE=1,
  ZAUTHENTICATED=1)

### System state at time of investigation

- Machine uptime: 9+ days
- Calendar processes (accountsd, calaccessd) running for 9+ days
- Multiple network adapters active: Tailscale VPN, Thunderbolt Ethernet, Wi-Fi
- No visible errors in account configuration

### Observable symptoms

- Calendar sync works correctly after restart
- Sync gradually stops working over days of uptime
- Old notifications appear in bulk after restart (indicating queued events that
  were not synced)
- Calendar processes are running but not actively syncing

### Most likely cause

- OAuth2 token refresh failures during network transitions
- Long-running calendar daemons accumulating failed connection state
- Network state changes (VPN, adapter switching, sleep/wake) correlating with
  multi-day uptime

### Immediate workaround

- Restart the machine, OR
- Kill calendar processes: `killall CalendarAgent calaccessd accountsd` (they
auto-restart)
<!-- SECTION:NOTES:END -->
