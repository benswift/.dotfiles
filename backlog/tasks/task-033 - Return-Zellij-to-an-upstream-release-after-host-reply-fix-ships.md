---
id: TASK-033
title: Return Zellij to an upstream release after host-reply fix ships
status: To Do
assignee: []
created_date: '2026-08-19 23:29'
updated_date: '2026-08-21 00:04'
labels:
  - maintenance
  - zellij
  - codex
dependencies: []
references:
  - 'https://github.com/zellij-org/zellij/issues/5365'
  - 'https://github.com/zellij-org/zellij/pull/5375'
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The dotfiles temporarily install Zellij from lbmeng/zellij revision 5f177f4 because v0.44.3 can misroute attach-time terminal replies into Codex and poison its input parser. Watch the upstream fix and remove the source-build workaround only after an official Zellij release contains equivalent host-reply isolation.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 An official Zellij release is verified to contain the host-reply isolation fix from issue #5365 / PR #5375 or an equivalent upstream implementation
- [ ] #2 mise/config.toml uses the normal prebuilt zellij = "latest" entry and no longer references the temporary lbmeng fork revision
- [ ] #3 The official release is installed on daysy and weddle, and fresh login shells resolve that release
- [ ] #4 Codex remains responsive after at least five zj-switch round trips on the released Zellij build
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## 2026-08-21 upstream check

Zellij v0.45.0 shipped 2026-08-20 (82 commits since v0.44.3) and does NOT contain the fix.

- issue #5365: still open, no maintainer response
- PR #5375: still open, unmerged, zero reviews since 2026-07-17, and now `mergeable_state: dirty` / not rebaseable against main after the 0.45.0 cycle
- the pinned rev 5f177f4 is exactly PR #5375's head, so the fork has not moved either
- nearest-sounding 0.45.0 commits are unrelated: #5163 (stale grid after forwarding a host reply), #5320 (attach-time garbage over slow links), #5446 (client keyboard-parser regression from #5323)

So AC #1 still fails and the cargo pin in mise/config.toml stays. Next check: watch for a maintainer touching #5375, or re-test whether plain v0.45.0 still reproduces on daysy/weddle.

### Rebase cost onto v0.45.0 (checked 2026-08-21)

Mechanical, not a rewrite. Merge base 5254e4fc; v0.45.0 is a strict descendant. Two conflict hunks, both caused by #5472 (98a08370, "allow opting-in to reading paste buffer"), which added a parallel clipboard forward path (OSC 52 capture, `resolve_async` on `ForwardQueryToHost`) through the same code the PR patches:

- `zellij-client/src/lib.rs` --- keep upstream's new `resolve_async` arm, re-apply the PR's `open_forward(token, &query_bytes)` on the fallback arm
- `zellij-client/src/stdin_ansi_parser.rs` --- keep upstream's clipboard-reply block, drop the unconditional OSC accumulate the PR already removes
- `zellij-client/src/stdin_ansi_parser_tests.rs` --- auto-merges with NO conflict markers but does not compile: two new upstream tests call the old one-arg `open_forward(2)`. Pass `BACKGROUND_QUERY` at both sites. A naive `git rebase --continue` ships this broken.
- `zellij-integration-tests/tests/startup_host_query.rs` --- no upstream churn, applies clean

Resulting diff vs v0.45.0 is the PR's intended footprint only (341+/71-, 4 files). Not yet compiled or tested.

### v0.45.0 verdict: bug confirmed still present (2026-08-21)

Tested deterministically rather than by hand. PR #5375 ships its own end-to-end regression test (`attach_startup_replies_are_not_forwarded_into_pane_query`, zellij's in-process harness + fake pty). Its diff is test-file-only and every harness symbol it needs already exists in v0.45.0, so it grafts onto a stock v0.45.0 worktree with no client changes.

- **stock v0.45.0: FAILS in 1.59s.** The pane received the whole attach burst --- `ESC[4;1160;2220t ESC[6;20;10t OSC11 OSC10 ESC[?2026;2$y OSC10` --- instead of only the answer to its own OSC 10 query. `ESC[?2026;2$y` is exactly the DECRPM report that leaves crossterm holding a CSI prefix and eating keystrokes.
- **rebased branch: PASSES.** `cargo test -p zellij-client stdin_ansi_parser` 85 passed / 0 failed; `--test startup_host_query` 5 passed / 0 failed.

So the rebase onto v0.45.0 is verified correct, not merely conflict-free. An ht-mcp-driven reproduction was tried first and abandoned: ht answers none of DA1 / OSC 10 / OSC 11 / DECRPM, so there are no replies to misroute and the test would be vacuous.
<!-- SECTION:NOTES:END -->
