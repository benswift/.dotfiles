---
id: TASK-023
title: Diagnose codex TUI freeze after zellij detach/reattach on daysy
status: To Do
assignee: []
created_date: '2026-07-13 07:05'
labels:
  - codex
  - zellij
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
codex CLI sometimes becomes non-responsive after detaching and reattaching a zellij session on daysy (ghostty). Symptom: UI mostly frozen but the cursor still blinks --- the blink is drawn by ghostty, so codex has stopped redrawing.

Investigated 2026-07-13 on weddle (identical versions: zellij 0.44.3, codex 0.144.1): NOT reproducible in a headless terminal in any variant --- clean detach while idle, detach mid-request, hard client kill, and with `tui.alternate_screen=always` forced. The trigger therefore involves ghostty as the attached client (kitty keyboard protocol / synchronized output / focus reporting --- capabilities a dumb terminal never advertises). No matching upstream issue found in openai/codex, zellij-org/zellij, or ghostty-org/ghostty.

Workaround under trial: `alternate_screen = "never"` in codex/config.toml (the dotfiles profile) --- inline mode was robust across every detach variant tested, and sidesteps zellij's alt-screen state restoration entirely.

Next time it freezes, run these three checks (each discriminates a failure mode):

1. `ps -o stat=,command= -p "$(pgrep -f codex | head -1)"` --- STAT `T` means the process was stopped; `kill -CONT <pid>` should revive it. Implicates codex suspend handling (bug class of openai/codex PR #28342).
2. Resize the ghostty window slightly --- if it unfreezes, it is the stale-terminal-size redraw bug (openai/codex #21978 family).
3. Blind-type while running `tail -f ~/.codex/log/codex-tui.log` --- if the log advances on keypresses, input is reaching codex and zellij is withholding the pane output (file against zellij, not codex).

Also record: codex + zellij versions at the time, whether the alternate_screen=never workaround was active, and what codex was doing when detached.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 next freeze on daysy is triaged with the three checks and the failure mode identified (process stopped / stale-size redraw / output withheld)
- [ ] #2 either an upstream issue is filed with the collected evidence, or the alternate_screen=never workaround is confirmed to prevent recurrence and documented in CLAUDE.md
<!-- AC:END -->
