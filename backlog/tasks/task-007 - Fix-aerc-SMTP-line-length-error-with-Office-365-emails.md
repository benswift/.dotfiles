---
id: task-007
title: Fix aerc SMTP line length error with Office 365 emails
status: To Do
assignee: []
created_date: '2025-08-26 00:21'
labels: []
dependencies: []
---

## Description

When replying to emails from Office 365/Outlook, aerc fails with 'smtp: too long a line in input stream' error. The issue is caused by Microsoft's X-Microsoft-Antispam-Message-Info header which contains extremely long base64-encoded lines that exceed the SMTP RFC 2822/5322 limit of 998 characters per line.

## Problem Details
- Error message: 'smtp: too long a line in input stream'
- Occurs when replying to emails from Office 365/Outlook
- Root cause: X-Microsoft-Antispam-Message-Info header has lines >998 chars
- aerc uses go-message library which strictly enforces RFC compliance
- Office 365 violates RFC standards with non-compliant headers

## Current Workarounds
1. Send as new message (C) instead of reply (r) - bypasses References header
2. Use DavMail gateway as proxy between aerc and Office 365
3. Route outgoing mail through different SMTP server

## Long-term Solutions
- aerc developers could relax RFC compliance for real-world compatibility
- Use header wrapping/sanitization before sending
- Filter out problematic headers during reply composition

## References
- Related issue: todo.sr.ht/~sircmpwn/aerc2/453 (malformed MIME headers)
- Discussion: lists.sr.ht/~rjarry/aerc-discuss (invalid message IDs)
- go-message issue #144 (Microsoft-Antispam headers)
