---
id: task-009
title: sanity check on anu Archive mailbox
status: To Do
assignee: []
created_date: "2025-08-28 11:28"
labels: []
dependencies: []
---

## Description

After some recent issues with a corrupted ANU maildir (in particular,
~/Maildir/anu/Archive) had lots of duplicated messages (which had also been
synced to the remote by mbsync).

A recent scripted fix has been completed. However, we also made a backup of the
folder in ~/Maildir/anu/anu-backup-20250828. As a sanity check, can you check
(using the python email library):

- whether there are any messages in the backup folder that aren't in the
  (freshly-synced) ~/Maildir/anu/Archive folder
- whether there are any duplicates in the (freshly-synced) ~/Maildir/anu/Archive
  folder

Don't make any changes, just report the results.
