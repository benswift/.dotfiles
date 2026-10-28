---
id: TASK-041.07
title: Collaborator release of the new engine in the lx vault
status: In Progress
assignee: []
created_date: '2026-10-28T22:19:00Z'
updated_date: '2026-10-28T23:40:00Z'
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
- [ ] #7 admin/tools/calendar-export, _generated/calendar.md and watson-run's calendar step are gone, and the lx calendar runs as the calendar workflow on watson
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
2026-10-29: everything up to the round trip is done. v0.10.0 is published (engine tag, builds on the lx remote's refs/tools with SHA256SUMS beside them, not pinned) and installs and validates on weddle and watson; admin/tools/wiz-install takes a version, for the round trip (on lx main). The switch is staged in ~/projects/wiz-lx-switch (README.md says how): rewritten CLAUDE.md, start-here.md, the four skills (/wiz-help takes over check.go's LX checks), watson-run (wiz due lx at the pin), wiz-check, the hooks and their test, mdbase.yaml, five meta design docs folding the twelve decisions (3, 6, 9 and 11 dropped, their live parts folded in), task-26 as a tasks/ record tagged meta, and the deletions (admin/tools/wiz, schemas, calendar-export, _generated/calendar.md, admin/backlog, backlog.config.yml). Dry-run on a scratch clone validated clean; watson ran the switched vault's workflows against a scratch remote, one commit each, exit 0. Waiting on Cole and Rob's round trip, then apply.sh, the unit's description, and the GitLab rename. Found while testing: commit 8fde84d1 (Cole, 2026-10-28) added j.cooney@anu.edu.au as an admin in admin/tools/identities, around the guards; watson's problems report flags it.
<!-- SECTION:NOTES:END -->
