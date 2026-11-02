---
id: TASK-041.06
title: Dotfiles cutover and nb's retirement on every host
status: Done
assignee: []
created_date: '2026-10-28T22:19:00Z'
updated_date: '2026-10-29T03:15:00Z'
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
- [x] #1 Home has run on the engine for a week without trouble before nb is removed
- [x] #2 No dotfiles file, unit, plist, test, mise pin or doc still invokes or describes nb, pkb-agent or pkb-todo, and dotfiles doctor verifies the engine instead
- [x] #3 The ben:pkb skill describes the wiz CLI, and a session following it can add a task, add a note about a project and run a workflow in home
- [x] #4 unit-oncall files an oncall task on a unit's failure and closes it on success, found by wiz query on its unit field
- [x] #5 Every host runs the engine and the wiz-due scheduler, and none runs nb-sync
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
2026-10-29: done on weddle --- ben:pkb rewritten for the wiz CLI (and phdconvenor, typst-poster, anu-theme-sync find the vault with wiz path); unit-oncall and launchd-run file and close home tasks by their unit field; bin/todo, note(), cpath and latest in zshrc; student-db via wiz path; the pdf and docx home workflows (bin/vault-render); pkb package, pkb-agent units and calendar-archive units and JSONL mode gone; bin/pkb-health; doctor checks the engine and the registry; CLAUDE.md, claude/CLAUDE.md and mail/utils/CLAUDE.md updated. Waiting: (1) nb itself (bin/nb, tests/test_nb.py, nb/*.nb-plugin and nb_plugin_links, nb-sync units and plist, the mise pin, doctor's nb check, the nb lines in CLAUDE.md and CI) once home has run a week on the engine, i.e. from 2026-11-05, together with the move to ~/vaults/wiz-home and ~/vaults/wiz-lx, the GitHub rename benswift/nb -> wiz-home and restic-backup's paths; (2) daysy, unreachable today: install com.benswift.wiz-due.plist, register the vaults, and unload and remove com.xwmx.nb-sync.plist. nb-sync is stopped and disabled on weddle; daysy's will still fire on its next wake, which pulls the migrated vault (a fast-forward unless daysy has unpushed nb edits, which would merge and want checking with wiz validate home).

2026-10-29: Ben's vault is named ben, not home; its repo becomes benswift/wiz-ben (staying on GitHub) and its path ~/vaults/wiz-ben when nb retires.

2026-10-29: daysy done --- nb-sync unloaded and its plist removed, wiz built at the pin, ben and lx registered, com.benswift.wiz-due installed and running, pkb-tools uninstalled. On waking, daysy's nb-sync had committed and pushed ten stale poster PDFs from docs/<slug>/ (nb's git add --all, after the move to typst/ left them unignored); removed, and the engine's deferred sync now leaves any new file nothing claims uncommitted, so the same can't happen through wiz. Watson's scheduler arrives with the switch.

2026-10-29: finished early at Ben's call, without the week's wait. Both vaults live in ~/vaults (wiz-ben, wiz-lx) on weddle and daysy, registered there, with Claude's session history moved to match; benswift/nb is benswift/wiz-ben on GitHub. nb is gone: bin/nb, tests/test_nb.py, the plugins and nb_plugin_links, nb-sync's units and plist, the mise pin, doctor's check, CI's ignore and the CLAUDE.md lines, plus ~/.nb, ~/.nbrc and a stray /usr/local/bin/nb on weddle. restic backs up ~/vaults; newsyslog rotates wiz-due.log in nb-sync.log's place (daysy needs the sudo cp from jobs.conf's header to pick that up). Doctor is clean on both hosts.
<!-- SECTION:NOTES:END -->
