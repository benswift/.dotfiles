---
id: TASK-040
title: Retire the rclone Maildir mirror and the 2026-10-24 mail quarantines
status: To Do
assignee: []
created_date: '2026-10-23T21:38:00Z'
due_date: '2026-10-31'
labels:
  - mail
  - cleanup
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
weddle has pulled mail itself since 2026-10-24 (mail/mbsyncrc-weddle). If a week of mail-pull runs has been clean, remove the rollbacks kept from the cutover:

- /data/Maildir, the frozen rclone mirror, and its line in bin/restic-backup's SOURCES, in the same commit (restic-backup fails on a missing source)
- /data/mail-quarantine/ on weddle (the three Deleted Items folders from the first, incomplete pull)
- ~/mail-quarantine/task025-20261024/ and ~/mail-quarantine/notmuch-db-20261024/ on daysy

Check first: journalctl --user -u mail-pull.service shows no failures, and each folder's server UID list (IMAP UID SEARCH ALL) matches the UIDs paired in weddle's .mbsyncstate.
<!-- SECTION:DESCRIPTION:END -->
