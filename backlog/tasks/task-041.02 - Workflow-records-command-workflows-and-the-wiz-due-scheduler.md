---
id: TASK-041.02
title: Workflow records, command workflows and the wiz due scheduler
status: To Do
assignee: []
created_date: '2026-10-28T22:19:00Z'
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
- [ ] #1 A command workflow whose requires: is unmet, or whose host: is another machine, reports itself unavailable rather than failing
- [ ] #2 One scheduler (wiz due --all, from one systemd timer on Linux and one launchd job on macOS) runs every due workflow for its host across all vaults, and a failed run on weddle files an oncall task through the unit's OnFailure wiring
- [ ] #3 The engine's source and tests contain no agent, prompt, profile or model concept; home's scheduled agent tasks run as plain command workflows via agent-run, with every run stamped, logged, timed out and failed through the same bookkeeping as any other workflow
- [ ] #4 agent-run reads its prompt from stdin when none is given, and --expect-changes exits non-zero when the working tree is unchanged, for both runners, with tests
- [ ] #5 pkb-agent runs nothing, and weddle's wiz-validate unit is gone with its oncall wiring carried over
<!-- AC:END -->
