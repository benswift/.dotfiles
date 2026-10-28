---
id: TASK-041
title: Archive Exchange mail server-side from neomutt instead of via mbsync upload
status: To Do
assignee: []
created_date: '2026-10-28T09:47:00Z'
updated_date: '2026-10-28T09:48:00Z'
labels:
  - mail
  - enhancement
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
neomutt archives by moving a message locally from INBOX to Archive. mbsync can't express that as a server move, so it APPENDs a new copy (new UID, X-TUID header added) and expunges the original. That is the upload path behind the old duplication loop (the X-TUID pairs found in the Online Archive), it re-uploads attachments in full (MaxSize 35m skips big ones, leaving them local-only), and Exchange sees a new message rather than a moved one.

Instead, have neomutt's archive key ask the server to move the message (IMAP UID MOVE, supported by Exchange Online) and let the next sync pull the result down: nothing uploaded, nothing deleted. Ben keeps everything (inbox zero via Archive, rarely deletes), so this must stay a move. Applies to the four Exchange accounts; Fastmail is fine as is.

Needs: neomutt message -> server UID via the folder's .mbsyncstate (local ,U= is the near UID), the account's IMAP login (bin/mail-secret oauth anu), and a fallback to the current local move when offline.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Archiving an Exchange message in neomutt moves it on the server with IMAP MOVE; the next sync shows no upload for it
- [ ] #2 Offline or on failure, it falls back to the current local move rather than losing the message
- [ ] #3 No message is ever deleted from the server by the new path
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Fastmail stays on the local move for now: it uploads fast, has no MaxSize problem, and threads by headers, so the gain is small. Make the server-move step per-account so Fastmail can opt in later (IMAP MOVE, or JMAP, which Fastmail API tokens allow for message moves).
<!-- SECTION:NOTES:END -->
