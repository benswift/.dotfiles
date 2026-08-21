---
id: TASK-033
title: Return Zellij to an upstream release after host-reply fix ships
status: To Do
assignee: []
created_date: '2026-08-19 23:29'
updated_date: '2026-08-21 00:39'
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
The dotfiles temporarily install Zellij from `benswift/zellij` revision `15240951`: PR #5375 rebased onto the v0.45.0 tag. Stock v0.45.0 can misroute attach-time terminal replies into Codex and poison its input parser. Keep the release-based fork as a local workaround, coordinate a clean main-based upstream contribution, and return to an official prebuilt release only after equivalent host-reply isolation ships.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 An official Zellij release is verified to contain the host-reply isolation fix from issue #5365 / PR #5375 or an equivalent upstream implementation
- [ ] #2 The official release is installed on daysy and weddle, and fresh login shells resolve that release
- [ ] #3 Codex remains responsive after at least five zj-switch round trips on the released Zellij build
- [ ] #4 mise/config.toml uses the normal prebuilt zellij = "latest" entry and no longer references any fork revision (was lbmeng's pre-0.45 branch; as of 2026-08-21 it is benswift/zellij host-reply-isolation-v0.45.0, i.e. PR #5375 rebased onto the v0.45.0 tag)
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
1. Keep `benswift/zellij:host-reply-isolation-v0.45.0` pinned at rev `15240951` as the local v0.45.0 workaround; do not use this release-based branch as an upstream PR head.
2. Before maintainer outreach, verify the pinned build remains installed and usable on daysy and weddle, including repeated `zj-switch` round trips.
3. Next week, comment on upstream PR #5375 with the independent v0.45.0 regression evidence and offer the clean transplant; ask @lbmeng whether they want to refresh their PR or are happy for a takeover.
4. Ask the Zellij maintainers via their recommended Discord/Matrix channel whether they are willing to review the fix, and whether they prefer the current small client-side reply matcher or an expected-reply discriminator carried over IPC.
5. Prefer updating the existing PR. Only if the original author is unresponsive and maintainers invite a takeover, create a separate branch from current `zellij-org/zellij:main`, transplant only the two authored commits, retain Bin Meng's authorship/sign-offs, and open a draft PR that credits and links #5375.
6. On the clean main-based branch, confirm the diff is limited to the four intended client/test files, then run format, the parser tests, the startup-host-query integration tests, and the broader upstream test suite requested by maintainers.
7. After an equivalent fix is merged and included in an official release, install that release on daysy and weddle, perform the task's five-round-trip checks, and restore `zellij = "latest"` in mise.
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
<!-- SECTION:NOTES:END -->
