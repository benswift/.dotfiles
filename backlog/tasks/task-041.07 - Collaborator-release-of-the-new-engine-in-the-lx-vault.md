---
id: TASK-041.07
title: Collaborator release of the new engine in the lx vault
status: In Progress
assignee: []
created_date: '2026-10-28T22:19:00Z'
updated_date: '2026-10-29T03:15:00Z'
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
- [x] #2 The lx vault's twelve decision records are folded into meta design docs stating current rules with their reasons and rejected alternatives, nothing in the vault describes a superseded design, admin/backlog/ and the backlog CLI are gone from the vault, and open wiz proposals are tasks/ records tagged meta
- [x] #3 The lx CLAUDE.md describes the wiz CLI and the new record types, and a session following it can add a task, add a note about a project and run a workflow
- [x] #4 start-here.md points at wiz help principles, and a colleague's Claude, asked why wiz works the way it does, answers from the principles and the meta docs
- [ ] #5 The GitLab project is u2548636/wiz-lx, the old path is unused, and Cole's, Rob's and watson's clones still work
- [x] #6 watson runs wiz due lx at the lx pin in place of watson-run's own steps, and weddle's wiz-validate unit is replaced with its oncall wiring carried over
- [x] #7 admin/tools/calendar-export, _generated/calendar.md and watson-run's calendar step are gone, and the lx calendar runs as the calendar workflow on watson
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
2026-10-29: everything up to the round trip is done. v0.10.0 is published (engine tag, builds on the lx remote's refs/tools with SHA256SUMS beside them, not pinned) and installs and validates on weddle and watson; admin/tools/wiz-install takes a version, for the round trip (on lx main). The switch is staged in ~/projects/wiz-lx-switch (README.md says how): rewritten CLAUDE.md, start-here.md, the four skills (/wiz-help takes over check.go's LX checks), watson-run (wiz due lx at the pin), wiz-check, the hooks and their test, mdbase.yaml, five meta design docs folding the twelve decisions (3, 6, 9 and 11 dropped, their live parts folded in), task-26 as a tasks/ record tagged meta, and the deletions (admin/tools/wiz, schemas, calendar-export, _generated/calendar.md, admin/backlog, backlog.config.yml). Dry-run on a scratch clone validated clean; watson ran the switched vault's workflows against a scratch remote, one commit each, exit 0. Waiting on Cole and Rob's round trip, then apply.sh, the unit's description, and the GitLab rename. Found while testing: commit 8fde84d1 (Cole, 2026-10-28) added j.cooney@anu.edu.au as an admin in admin/tools/identities, around the guards; watson's problems report flags it.

2026-10-29: watson's scheduler is live ahead of the switch. lx's interim admin/tools/watson-run installs v0.10.0 by name and runs wiz due lx (mail, calendar, validate, weekly skills review, stamped so its first run is next Monday), then still writes _generated/calendar.md for the v0.9.1 dancecard. events/ is automation's in mdbase.yaml, and the lxconvenor calendar is 76 event records on main. Until the switch, a deliveries report asked for with v0.9.1's wiz job (an old-style task assigned to watson) isn't picked up; run it as wiz job lx deliveries-report from a v0.10.0 machine instead. The staged watson-run is the final one.

2026-10-29: switched at Ben's call ahead of the round trip, which Cole and Rob now do as their move onto it. 30f31d66 is the switch (v0.10.0 pinned); deleting _generated/calendar.md went in separately as automation (aa659b9f), since the commit guard keeps that path to automation. watson ran the final watson-run by hand (exit 0) before the timer resumed. The GitLab project is u2548636/wiz-lx; weddle's, daysy's and watson's clones point there. wiz check doesn't follow the move by itself: GitLab redirects the old SSH path silently and prints its move notice only on a push after the old path was used, so Cole's and Rob's instructions set the remote. Open: AC1 and AC5 close when Cole and Rob have pulled, installed v0.10.0, set the remote and shared an edit.
<!-- SECTION:NOTES:END -->
