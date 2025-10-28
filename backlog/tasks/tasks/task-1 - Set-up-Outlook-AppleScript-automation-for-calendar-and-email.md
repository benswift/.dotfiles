---
id: task-1
title: Set up Outlook AppleScript automation for calendar and email
status: To Do
assignee: []
created_date: "2025-08-29 13:25"
labels: []
dependencies: []
---

## Description

Configure Outlook for Mac with ANU Exchange account to enable AppleScript
automation for calendar access and Archive folder migration

## Context

Currently using mbsync/msmtp for email but need programmatic calendar access.
PowerShell with Exchange Online has limited capabilities (no event read/write).
Graph API requires complex OAuth setup. AppleScript with Outlook provides full
access but requires Outlook to be configured with the Exchange account.

## Implementation steps

1. **Configure Outlook with ANU Exchange account**

   - Open Outlook for Mac
   - Add Exchange account: u2548636@anu.edu.au
   - Authenticate with ANU credentials
   - Wait for initial sync to complete

2. **Test AppleScript access**

   - Run `~/.dotfiles/mail/simple-outlook-test.applescript` to verify connection
   - Confirm calendar and email folders are accessible

3. **Enable calendar automation**

   - Test `outlook-cal` commands:
     - `outlook-cal today` - view today's events
     - `outlook-cal week` - view week's events
     - `outlook-cal create "Test Event" 2025-09-01 14:00` - create events
     - `outlook-cal search "meeting"` - search calendar

4. **Set up Archive migration**
   - Create Archive2 folder in Outlook
   - Test batch move: `outlook-cal move-archive` (moves 100 messages)
   - Plan full migration: `outlook-cal batch-move 700` (for 70k messages)

## Success criteria

- Can read calendar events via AppleScript
- Can create new calendar events programmatically
- Can batch move emails from Archive to Archive2 folder
- Scripts are documented and added to dotfiles

## Benefits

- Full calendar read/write access without complex OAuth
- Ability to automate email folder operations
- No additional API keys or app registrations needed
- Works with existing ANU Exchange authentication

## Files created

- `~/.dotfiles/mail/outlook-calendar.applescript` - Main AppleScript library
- `~/.dotfiles/mail/outlook-cal` - Shell wrapper for easy CLI access
- `~/.dotfiles/mail/archive-migration-tracker.sh` - Progress tracker for email
  migration
