---
id: task-005
title: >-
  build mbsync from source, and XOAUTH2 plugin as well, can we get it upstreamed
  to homebrew
status: To Do
assignee: []
created_date: "2025-08-23 13:01"
labels: []
dependencies: []
---

## Description

The homebrew-procvided mbsync formula (called isync for legacy reasons - file is
here /opt/homebrew/Library/Taps/homebrew/homebrew-core/Formula/i/isync.rb)
doesn't have the XOAUTH2 plugin.

In the @mbsync-build/ directory (and also in /tmp/cyrus-sasl-xoauth2/) I have
built (and make installed) an mbsync with the XOAUTH2 plugin.

Examine the folders, source code and built artefacts to determine:

- could the cyrus-sasl-xoauth2 plugin be upstreamed to homebrew?
- what changes are required to the mbsync formula upstream the XOAUTH2 plugin to
  homebrew?
