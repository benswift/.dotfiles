---
id: TASK-019
title: Switch from prettier to oxfmt for markdown formatting
status: To Do
assignee: []
created_date: '2026-03-03 22:26'
labels:
  - tooling
  - blocked-upstream
dependencies: []
references:
  - 'https://oxc.rs/docs/guide/usage/formatter.html'
  - helix/languages.toml
  - zshrc
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Replace prettier with oxfmt (https://oxc.rs/docs/guide/usage/formatter.html) for markdown formatting once oxfmt reaches v1.0. This affects:

- helix markdown formatter in `helix/languages.toml` (currently `prettier --prose-wrap always`)
- the `prettify-md` alias in `zshrc`

oxfmt is part of the oxc toolchain and should be significantly faster than prettier. Wait for v1.0 stability before switching.
<!-- SECTION:DESCRIPTION:END -->
