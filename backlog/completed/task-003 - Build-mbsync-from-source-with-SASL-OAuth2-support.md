---
id: task-003
title: Build mbsync from source with SASL/OAuth2 support
status: Done
assignee: []
created_date: "2025-08-23 11:22"
labels: []
dependencies: []
---

## Description

Build mbsync (isync) from source with cyrus-sasl support to enable OAuth2
authentication for Office 365. Use the latest stable release from
https://sourceforge.net/projects/isync/. Required dependencies: berkeley-db,
openssl, cyrus-sasl. Configure with --with-sasl flag. This will replace the
Homebrew version which lacks OAuth2 support.
