---
id: TASK-041.07
title: Collaborator release of the new engine in the lx vault
status: To Do
assignee: []
created_date: '2026-10-28T22:19:00Z'
updated_date: '2026-10-28T22:46:00Z'
labels:
  - pkb
  - wiz
dependencies:
  - TASK-041.03
  - TASK-041.06
parent_task_id: TASK-041
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Step 7 of TASK-041's order of work. Groundwork (_types/, mdbase.yaml, events/, workflow records, meta docs) may land on main earlier because v0.9 ignores it; nothing may change CLAUDE.md or a skill to describe the new CLI before the switch.

- the switch is one push: the pin bump with the rewritten CLAUDE.md, skills (/wiz-help taking over check.go's LX diagnosis), start-here.md and watson-run, with weddle's wiz-validate paused for the hour
- watson: registry entry and the engine built at the pin; watson-run reduced to wiz due lx
- fold the twelve decision records into meta docs, move any open backlog task to tasks/ tagged meta, delete admin/backlog/ and backlog.config.yml, and drop CLAUDE.md's developing-wiz carve-out
- rename the GitLab project u2548636/wiz to wiz-lx only on Ben's explicit go-ahead (outward-facing); the old path is never reused, and wiz check rewrites remotes
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Cole and Rob each complete a get-latest, edit and share round trip on their own machines with the new release before it is pinned in the lx vault, and their slash commands keep their names
- [ ] #2 The lx vault's twelve decision records are folded into meta design docs stating current rules with their reasons and rejected alternatives, nothing in the vault describes a superseded design, admin/backlog/ and the backlog CLI are gone from the vault, and open wiz proposals are tasks/ records tagged meta
- [ ] #3 The lx CLAUDE.md describes the wiz CLI and the new record types, and a session following it can add a task, add a note about a project and run a workflow
- [ ] #4 start-here.md points at wiz help principles, and a colleague's Claude, asked why wiz works the way it does, answers from the principles and the meta docs
- [ ] #5 The GitLab project is u2548636/wiz-lx, the old path is unused, and Cole's, Rob's and watson's clones still work
- [ ] #6 watson runs wiz due lx at the lx pin in place of watson-run's own steps, and weddle's wiz-validate unit is replaced with its oncall wiring carried over
<!-- AC:END -->
