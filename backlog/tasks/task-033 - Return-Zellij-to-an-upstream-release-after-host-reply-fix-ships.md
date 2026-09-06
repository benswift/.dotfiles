---
id: TASK-033
title: Return Zellij to an upstream release after host-reply fix ships
status: To Do
assignee: []
created_date: '2026-08-19 23:29'
updated_date: '2026-09-06 04:37'
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
The dotfiles temporarily installed Zellij from `benswift/zellij` (PR #5375 rebased onto the v0.45.0 tag) because stock Zellij misroutes attach-time terminal replies into the focused pane and Codex < 0.148 wedged its input parser on the stray DECRPM report. Codex >= 0.148.0 discards that reply, which Ben accepted on 2026-09-06 as sufficient: return to the official prebuilt release now, and keep this task open only to see the Zellij-side fix upstream (or confirm it is moot).
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 The official release is installed on daysy and weddle, and fresh login shells resolve that release
- [ ] #2 Codex remains responsive after at least five zj-switch round trips on the released Zellij build
- [x] #3 mise/config.toml uses the normal prebuilt zellij = "latest" entry and no longer references any fork revision (was lbmeng's pre-0.45 branch; as of 2026-08-21 it is benswift/zellij host-reply-isolation-v0.45.0, i.e. PR #5375 rebased onto the v0.45.0 tag)
- [x] #4 Codex on the installed release stays responsive to the misrouted attach burst: either an official Zellij release contains host-reply isolation (#5365 / #5375 or equivalent) OR the installed Codex is >= 0.148.0, which discards the stray reply (verified 2026-09-06)
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
1. mise now uses prebuilt `zellij = "latest"` (v0.45.1 installed on weddle 2026-09-06). Still needed: `mise install zellij` on daysy, then five zj-switch round trips with codex >= 0.148 on each machine from a real ghostty client.
2. Post one short comment on #5375 (not a new PR): independent v0.45.0 and v0.45.1 evidence, the exact command that reproduces the failure (`cargo test -p zellij-integration-tests --test startup_host_query` with the PR's test grafted onto the tag), and that Codex now tolerates the misroute but stock-crossterm TUIs (crossterm #1104) do not. Ask a maintainer to approve the CI run.
3. Ask on the Zellij Discord #contributing/general whether imsnif still intends to remove the client startup query (his #5236 plan). If yes, that removal makes #5365/#5375 moot and is the PR he would actually take; offer to do it. If no, ask whether he wants #5375's reply matcher or a smaller server-side discriminator so the PR can be reshaped before review.
4. Do not open a competing PR or a takeover while lbmeng is actively rebasing; only transplant onto main if they go quiet and a maintainer invites it (keep authorship/sign-offs).
5. Cross-link #5557 in the outreach as the severe form of the same attach-time query traffic problem.
<!-- SECTION:PLAN:END -->

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

## 2026-08-21 acceptance review and local verification

The implementation remains technically persuasive: it isolates replies by the server's existing non-clipboard host-query whitelist, preserves the separate OSC 52 path added in #5472, and has deterministic unit and end-to-end coverage for the captured attach-time reply burst.

The current fork branch is intentionally a local workaround only. Because the v0.45.0 release tag is not an ancestor of upstream `main`, proposing `host-reply-isolation-v0.45.0` directly would show 20 changed files, including release metadata and bundled WASM assets. A temporary transplant of its two commits onto current upstream `main` applied cleanly and produced the intended four-file diff (341 additions, 71 deletions). No upstream branch or PR was created.

On weddle, mise resolves Zellij to the exact configured fork revision:
`~/.local/share/mise/installs/cargo-https-github-com-benswift-zellij/rev-1524095119990bc7af283296bbe291a3b4cffbcd/bin/zellij`
and the binary reports `zellij 0.45.0`.

The non-disruptive `mise exec -- zellij setup --check` smoke check also passed: the live config is well-defined and the bundled default plugins are available.

## 2026-09-06 upstream check

Zellij v0.45.1 shipped 2026-08-28 and still does NOT contain the fix. Verified the same way as before: PR #5375's regression test grafted test-only onto the v0.45.1 tag fails in 1.9s with the pane receiving the whole attach burst (`ESC[4;1160;2220t ESC[6;20;10t OSC11 OSC10 ESC[?2026;2$y OSC10`). Nothing in v0.45.0..v0.45.1 touches the forwarding path (#5523 edits stdin_ansi_parser.rs for OSC 9 notifications only).

- PR #5375: lbmeng force-pushed 2026-08-25, rebased onto main at b0bd3e1e (post-0.45.0, pre-0.45.1); now 2 ahead / 14 behind main. Still zero comments, zero reviews, and the CI workflows are stuck at `action_required` (never approved to run). Author is active elsewhere on GitHub daily.
- issue #5365: unchanged since 2026-07-14, no maintainer response
- new neighbours, also unanswered: #5557 (server SIGABRT on attach when a booted Codex pane is focused, 2026-08-29) and #5373 (Codex freeze after resize-while-detached, opencode reporter added 2026-08-25)

### Codex has worked around it (from 0.148.0)

Codex swapped crossterm for `openai-oss-forks/crossterm` branch `charlie/preserve-startup-terminal-input` in PR #38641 "Harden TUI startup input handling" (merged 2026-08-14, shipped in codex 0.148.0 on 2026-08-18). That branch's commit f4f65b19 "Discard completed unsupported terminal control sequences" makes the reader clear its buffer on any completed-but-unrecognised CSI or OSC instead of treating it as incomplete; its own unit test covers a DECRPM reply (`ESC[?1;2$y`). Upstream crossterm has the same bug filed as #1104 with fix PR #1106 (open, unreviewed), so stock-crossterm TUIs are still exposed.

Verified with a pty harness (scratchpad `inject_burst.py`): boot codex, write the captured burst into its stdin, then type and send Ctrl-C.

- codex 0.147.0 + burst: no output on typing, Ctrl-C ignored (wedged). Without burst: responsive.
- codex 0.153.4 + burst: responsive, Ctrl-C quits. Same as control.

So with codex >= 0.148.0 the zellij misroute is harmless garbage rather than a wedge; the fork pin is no longer load-bearing for the Codex symptom.

### Contribution-process read

CONTRIBUTING.md says minor fixes "might take a long while" and to ask on Discord/Matrix first. Observed behaviour is more specific: imsnif merges small external bug fixes in sweeps (2026-08-10/11, 08-17, 08-25/28), typically within days when the diff is small and the reproduction is one command, and usually hand-adjusts them before merging (#5446, a client stdin-parser fix, merged in 4 days). He pushes back on anything in the host-query/stdin path that conflicts with his own plans: #5236 (3-line startup palette query batching) was declined because "I was planning on removing the startup query entirely since we now also do this on demand", and #4882 (forward unrecognised stdin bytes) as "naive". #5375 is 341+/71- in exactly that path, so it is the shape of PR he leaves sitting. 344 PRs are open, oldest from 2021; neither Codex-related issue nor the PR has had any maintainer touch in 7 weeks.

## 2026-09-06 returned to the prebuilt release

Ben accepted the Codex-side fix as sufficient. mise/config.toml is back to `zellij = "latest"`; weddle installed v0.45.1 and a fresh login shell resolves `~/.local/share/mise/installs/zellij/latest/zellij` (`zellij 0.45.1`). The fork install dirs under `~/.local/share/mise/installs/cargo-https-github-com-*-zellij` are now orphaned and can go with `mise prune`. daysy and the five round trips remain.
<!-- SECTION:NOTES:END -->
