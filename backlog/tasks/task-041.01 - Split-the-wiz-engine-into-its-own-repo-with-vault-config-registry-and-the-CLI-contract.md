---
id: TASK-041.01
title: >-
  Split the wiz engine into its own repo with vault config, registry and the CLI
  contract
status: To Do
assignee: []
created_date: '2026-10-28T22:19:00Z'
labels:
  - pkb
  - wiz
dependencies: []
parent_task_id: TASK-041
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Step 1 of TASK-041's order of work; the parent holds the design.

- create u2548636/wiz-engine from the lx vault's admin/tools/wiz with history (git filter-repo --subdirectory-filter); binaries keep shipping to the lx remote's refs/tools/
- vault config: mdbase.yaml with a format version and name, _types/ with path-glob matching, and the vault rules (links_out, records vs assets, identities and protected paths including the always-admin-only set, sync policy, gitignored paths invisible)
- registry at ~/.config/wiz/vaults.toml, falling back to the vault containing the working directory
- the CLI contract: explicit vault names or --all, stable --json, and query (CEL via cel-go), show, path, add, update, append, rename, validate, sync and guard
- commit-touched-files model with per-vault commit policy
- PRINCIPLES.md, wiz help principles and wiz brief
- check.go split: generic Git, SSH and engine checks stay; the LX diagnosis leaves the engine (its /wiz-help text lands with the switch, TASK-041 step 7)
- lx vault: convert admin/tools/schemas/*.json to _types/ plus mdbase.yaml on main, which the pinned v0.9 binary ignores
- dotfiles: bin/wiz-build and wiz/pinned-rev, modelled on helix-build, installing into ~/.local/bin

nb keeps serving home. Don't rename the GitLab project here; that's step 7.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 The engine lives in its own repo with its history, and holds no LX-specific types, folders or logic beyond dancecard.go and calendar.go, which TASK-041 step 3 removes
- [ ] #2 The lx vault validates with the same results under the new engine as under the pinned v0.9 release, before any record changes
- [ ] #3 Every CLI command takes explicit vault names or --all, resolved via ~/.config/wiz/vaults.toml, with stable --json output; there is no current-notebook state
- [ ] #4 No engine commit ever holds a file the write that made it didn't touch (plus link-repaired files on rename); nothing runs git add --all; whether a write commits at once is per-vault policy
- [ ] #5 PRINCIPLES.md lives in the engine repo and wiz help principles prints it; wiz brief points at it
- [ ] #6 bin/wiz-build builds the pinned rev into ~/.local/bin, and dotfiles doctor reports whether the installed engine matches the pin
- [ ] #7 Cole and Rob see no change: their pin, CLAUDE.md and skills are untouched
<!-- AC:END -->
