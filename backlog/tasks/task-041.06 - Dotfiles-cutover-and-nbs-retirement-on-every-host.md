---
id: TASK-041.06
title: Dotfiles cutover and nb's retirement on every host
status: To Do
assignee: []
created_date: '2026-10-28T22:19:00Z'
labels:
  - pkb
  - wiz
dependencies:
  - TASK-041.05
parent_task_id: TASK-041
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Step 6 of TASK-041's order of work: the remaining items under 'This dotfiles repo' in the parent, the ben:pkb skill rewrite, and the hosts.

- ben:pkb (SKILL.md, email.md) rewritten for the wiz CLI and shrunk to what's Ben's, then dotfiles update
- unit-oncall and launchd-run's failure path via wiz add/query/update with a unit field on home's task type
- mail/utils student-db, zshrc (note function, todo alias), the pdf/docx plugins as home command workflows, cpath/pager/lumis-highlight as shell functions
- delete bin/nb, tests/test_nb.py, nb plugins and nb_plugin_links, nb-sync units and plist, pkb-todo, pkb-agent and the pkb package (CI, install.sh, dotfiles update)
- drop the nb mise pin; doctor verifies the engine; docs (CLAUDE.md, claude/CLAUDE.md, pkb/README.md)
- Ben's paths move to ~/vaults/wiz-lx and ~/vaults/wiz-home, and home's repo is renamed wiz-home
- daysy and any other Mac: registry, engine, launchd scheduler, nb-sync plist removed

Keep the nb pin and units as the rollback until home has run on the engine for a week without trouble.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Home has run on the engine for a week without trouble before nb is removed
- [ ] #2 No dotfiles file, unit, plist, test, mise pin or doc still invokes or describes nb, pkb-agent or pkb-todo, and dotfiles doctor verifies the engine instead
- [ ] #3 The ben:pkb skill describes the wiz CLI, and a session following it can add a task, add a note about a project and run a workflow in home
- [ ] #4 unit-oncall files an oncall task on a unit's failure and closes it on success, found by wiz query on its unit field
- [ ] #5 Every host runs the engine and the wiz-due scheduler, and none runs nb-sync
<!-- AC:END -->
