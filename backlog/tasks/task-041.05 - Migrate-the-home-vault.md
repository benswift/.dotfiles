---
id: TASK-041.05
title: Migrate the home vault
status: To Do
assignee: []
created_date: '2026-10-28T22:19:00Z'
updated_date: '2026-10-28T23:01:00Z'
labels:
  - pkb
  - wiz
dependencies:
  - TASK-041.02
  - TASK-041.04
parent_task_id: TASK-041
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Step 5 of TASK-041's order of work; 'Home vault migration (do it properly)' in the parent is the target schema.

A one-off uv script outside both repos, with a dry run that reports every change, committing one conversion per commit. Before applying: pause nb-sync on weddle and daysy and pkb-agent on weddle, push every host's home clone, and run the dry run against a copy until it is clean. Then rewrite home's CLAUDE.md for the wiz CLI and the new record types.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Ben has reviewed the dry-run report against a copy before the migration is applied
- [ ] #2 No *.todo.md, *.bookmark.md, YYYYMMDD.md or .index files remain, every tags field is a YAML list, and wiz validate home reports no problems
- [ ] #3 Home has no daily, bookmark or role type; its docs/ holds doc records and typst/ the Typst sources
- [ ] #4 Done todos are tasks with status: done, named by their nb creation date
- [ ] #5 Home's CLAUDE.md describes the wiz CLI and the new record types, and tells no agent to append to a day file
- [ ] #6 Home's health check and retrospective run as command workflows (the retrospective via agent-run --expect-changes with the prompt as its body), and pkb-agent runs nothing
- [ ] #7 Home's events/ holds event records written by calendar-archive as a command workflow (seeded once from data/calendars/*.jsonl with --seed), the JSONL files and the calendar-archive timer are gone, and calendar-archive has no JSONL mode left
<!-- AC:END -->
