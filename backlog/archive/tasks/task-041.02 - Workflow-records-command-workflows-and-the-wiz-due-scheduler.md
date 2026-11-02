---
id: TASK-041.02
title: Workflow records, command workflows and the wiz due scheduler
status: Done
assignee: []
created_date: '2026-10-28T22:19:00Z'
updated_date: '2026-10-28T22:46:00Z'
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
Step 2 of TASK-041's order of work (see 'Workflows: exactly two kinds' and the dotfiles items in the parent). This absorbs pkb-agent.

- engine: the closed workflow type, command workflows with the stdin/env/exit-code contract, requires: and host: availability, run, due with its bookkeeping (machine-local stamps, logs under ~/.local/state/wiz/logs/, timeout, sync around a batch, non-zero exit naming each failure), and job generalised to any workflow with a host:
- the guard keeps command workflows admin-only
- dotfiles: agent-run reads the prompt from stdin and gains --expect-changes; pkb-health becomes bin/pkb-health (a uv script in ruff's extend-include); wiz-due systemd units and a launchd plist with oncall wiring under the mise environment, the plist surviving a DarkWake; wiz-validate replaced by weddle running wiz due lx on watson over ssh
- lx: watson's jobs as command workflows (the deliveries report with requires: [typst] and host: watson, fed by wiz query --json; the skills review)
- home: health and claude-retrospective as command workflows, and pkb-agent stops running them
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 A command workflow whose requires: is unmet, or whose host: is another machine, reports itself unavailable rather than failing
- [x] #2 One scheduler (wiz due --all, from one systemd timer on Linux and one launchd job on macOS) runs every due workflow for its host across all vaults, and a failed run on weddle files an oncall task through the unit's OnFailure wiring
- [x] #3 agent-run reads its prompt from stdin when none is given, and --expect-changes exits non-zero when the working tree is unchanged, for both runners, with tests
- [x] #4 The engine's source and tests contain no agent, prompt, profile or model concept, and every run is stamped, logged, timed out and failed through the same bookkeeping
<!-- AC:END -->

## Final Summary

<!-- SECTION:FINAL_SUMMARY:BEGIN -->
Engine: workflow and request records are built-in types; wiz workflows/run/due/job; stamps and logs under ~/.local/state/wiz, one commit per run, sync around a batch, WIZ_VAULT/WIZ_VAULT_ROOT/WIZ_WORKFLOW/WIZ_LAST_RUN. The old Typst jobs left the engine. dotfiles: agent-run reads stdin and has --expect-changes; wiz-due systemd units (enabled on weddle) and launchd plist. lx: mail, validate, deliveries-report and skills-review command workflows under admin/tools/workflows (groundwork; verified on watson against a scratch clone). Home's tasks move with the home migration (TASK-041.05), since home isn't a vault until then; watson's cutover from watson-run to wiz due lx moves to the switch (TASK-041.07), since watson runs the lx pin.
<!-- SECTION:FINAL_SUMMARY:END -->
