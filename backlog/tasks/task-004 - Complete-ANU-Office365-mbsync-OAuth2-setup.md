---
id: task-004
title: Complete ANU Office365 mbsync OAuth2 setup
status: Completed
assignee: []
created_date: '2025-08-23 11:33'
labels: []
dependencies: []
---

## Description

Status: OAuth2 token successfully obtained via mutt_oauth2.py using devicecode flow with Thunderbird client ID (9e5f94bc-e8a4-4e73-b8be-63364c29d753). Token stored in ~/.dotfiles/anu_oauth2_token.

Current mbsyncrc configuration for ANU:
- Host: outlook.office365.com, Port: 993
- AuthMech: XOAUTH2
- PassCmd: ~/.dotfiles/mutt_oauth2.py --encryption-pipe 'cat' --decryption-pipe 'cat' ~/.dotfiles/anu_oauth2_token

## Resolution

Successfully configured mbsync with OAuth2 authentication for Office365:

1. Built and installed cyrus-sasl-xoauth2 plugin from https://github.com/moriyoshi/cyrus-sasl-xoauth2
2. OAuth2 token obtained via mutt_oauth2.py using Thunderbird client ID
3. mbsync now successfully authenticates and syncs with XOAUTH2 mechanism

The key was installing the XOAUTH2 SASL plugin which isn't included in standard cyrus-sasl distributions.

Note: Fastmail config still needs app password added to macOS keychain.
