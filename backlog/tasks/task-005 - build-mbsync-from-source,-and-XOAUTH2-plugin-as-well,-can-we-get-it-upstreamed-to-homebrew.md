---
id: task-005
title: >-
  Create Homebrew formulae for mbsync with XOAUTH2 support and upstream to
  homebrew-core
status: To Do
assignee: []
created_date: "2025-08-23 13:01"
labels: []
dependencies: []
---

## Description

The current Homebrew-provided mbsync formula (called isync for legacy reasons -
located at /opt/homebrew/Library/Taps/homebrew/homebrew-core/Formula/i/isync.rb)
doesn't include XOAUTH2 authentication support, which is required for modern
OAuth2-based email services like Office 365.

I've successfully built mbsync from source with XOAUTH2 support (see
@mbsync-build/ directory), which required the cyrus-sasl-xoauth2 plugin (built
in /tmp/cyrus-sasl-xoauth2/). This proves the functionality works, but requires
manual compilation.

## Goal

Create and upstream Homebrew formulae to enable XOAUTH2 support in mbsync:

1. **Create a cyrus-sasl-xoauth2 formula** for Homebrew (if it doesn't exist)

   - Package the cyrus-sasl-xoauth2 plugin as a Homebrew formula
   - Ensure it integrates properly with the existing cyrus-sasl formula

2. **Modify the isync (mbsync) formula** to:

   - Add optional XOAUTH2 support (possibly as a build option or variant)
   - Depend on cyrus-sasl-xoauth2 when the option is enabled
   - Ensure proper SASL plugin discovery at runtime

3. **Submit pull requests** to homebrew-core:
   - Follow Homebrew contribution guidelines
   - Create separate PRs for each formula if needed
   - Include test cases and documentation

## Resources

- Working source build: @mbsync-build/
- Homebrew contribution docs:
  - https://raw.githubusercontent.com/Homebrew/homebrew-core/refs/heads/main/CONTRIBUTING.md
  - https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request
- Current isync formula:
  /opt/homebrew/Library/Taps/homebrew/homebrew-core/Formula/i/isync.rb
- cyrus-sasl-xoauth2 source: https://github.com/moriyoshi/cyrus-sasl-xoauth2

## Success criteria

- Users can install mbsync with XOAUTH2 support via:
  `brew install isync --with-xoauth2` (or similar)
- No manual compilation required
- Changes accepted upstream to homebrew-core
