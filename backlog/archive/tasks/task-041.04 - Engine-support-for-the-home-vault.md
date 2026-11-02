---
id: TASK-041.04
title: Engine support for the home vault
status: Done
assignee: []
created_date: '2026-10-28T22:19:00Z'
updated_date: '2026-10-28T23:02:00Z'
labels:
  - pkb
  - wiz
dependencies:
  - TASK-041.01
parent_task_id: TASK-041
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Step 4 of TASK-041's order of work: what home needs beyond lx.

- wiz add with the body on stdin, as the capture operation
- the note catch-all as a lowest-precedence glob over the root
- unmatched records reported unless a type claims them (v0.9 silently skips untyped folders)
- cross-vault links ([[lx:people/...]]) resolved through registry names, and links_out: none enforced
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 wiz add home note with the body on stdin writes a valid note and commits it according to home's sync policy
- [x] #2 An unclaimed Markdown file at home's root validates as a note, and a record in a folder no type claims is reported
- [x] #3 A home record linking [[lx:people/...]] validates, and a link out of lx is a validation error
<!-- AC:END -->

## Final Summary

<!-- SECTION:FINAL_SUMMARY:BEGIN -->
add with the body on stdin, the note catch-all (priority) and the unclaimed-Markdown report came with step 1; cross-vault links resolve through the registry, and links_out: none refuses them. Tested in cli_test.go.
<!-- SECTION:FINAL_SUMMARY:END -->
