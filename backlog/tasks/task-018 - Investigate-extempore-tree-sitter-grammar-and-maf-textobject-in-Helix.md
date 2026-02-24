---
id: TASK-018
title: Investigate extempore tree-sitter grammar and maf textobject in Helix
status: To Do
assignee: []
created_date: '2026-02-24 07:23'
labels:
  - helix
  - extempore
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The `maf` (match around function) textobject for extempore files doesn't work in Helix. The textobject query exists at `helix/runtime/queries/extempore/textobjects.scm` and the grammar source is defined in `helix/languages.toml`, but the compiled grammar may not be built on this machine.

Steps to investigate:
- Check if `hx --grammar fetch && hx --grammar build` has been run and whether the extempore grammar compiled successfully
- Verify the textobjects.scm query matches correctly once the grammar is available
- Also check whether `Ctrl-Enter` (C-ret) works correctly for `:pipe-to xtm-send` in normal terminal usage (it failed through headless-terminal MCP but may work fine in a real terminal)
<!-- SECTION:DESCRIPTION:END -->
