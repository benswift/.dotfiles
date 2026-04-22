---
id: TASK-020
title: Harness scaffolding from session log analysis
status: To Do
assignee: []
created_date: "2026-04-07"
labels:
  - tooling
  - claude-code
dependencies: []
references:
  - claude/settings.json
  - claude/CLAUDE.md
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Analysis of 2,871 Claude Code sessions (1.7 GB of logs, 77 projects, ~116k user
turns, ~98k tool calls) reveals several patterns that could be addressed with
harness-level scaffolding --- hooks, instructions, or settings changes.

<!-- SECTION:DESCRIPTION:END -->

## Findings

### Usage profile

- 70.8% of sessions have 10+ user turns; median is 24 turns. Only 8% are
  single-shot. Sessions are deep and iterative.
- Median duration 9 min, mean 73 min. Heavy tail of 260 sessions over 2 hours.
- 92% of sessions are multi-turn. The harness should optimise for long-running,
  iterative workflows, not one-off queries.

### Error patterns (5.8% overall, 51.9% of sessions hit at least one)

| Category                        | Count | Notes                                                   |
| ------------------------------- | ----- | ------------------------------------------------------- |
| Non-zero exit                   | 863   | Bash command failures (builds, tests, bad commands)     |
| File not found                  | 823   | Agent guessing at paths that don't exist                |
| Edit match failure              | 347   | `old_string` not unique or stale file model             |
| Write without read              | 293   | Pure protocol violation --- agent writes before reading |
| File doesn't exist (Write/Edit) | 259   | Attempting to modify nonexistent files                  |
| Timeout                         | 228   | Long commands exceeding 2 min default                   |
| User rejected tool              | 131   | Permission prompts breaking agent flow                  |

The top four categories (2,326 errors, 41% of all errors) are preventable with
pre-flight checks.

### Tool usage

- Bash (28.6%), Edit (12.3%), Read (11.3%) dominate. Sub-agent proxy
  (`mcp__acp__*`) tools account for 21% of all calls.
- Edit:Write ratio is 6:1 --- the "prefer editing" instruction works.
- Tool parallelism is essentially 0%. Tool calls are purely sequential, even
  when independent (e.g. Read → Read, Grep → Grep chains).
- TodoWrite acts as a workflow checkpoint between tool phases.

### Tool chains (most common bigrams)

- Bash → Bash (18,428) --- long shell command sequences
- Edit → Edit (5,664) --- multi-file changes
- Read → Read (3,690) --- exploring multiple files
- Edit → Bash (3,246) --- the "edit then run" loop
- Read → Edit (2,576) --- read then modify

### Session modes

- Yolo mode: 5.0% error rate, avg 49 turns. Default mode: 6.2% error rate, avg
  37 turns. Permission interruptions appear to increase error rates slightly.
- Thinking mode sessions average 54.7 turns vs 33.7 without --- thinking
  correlates with harder tasks.
- 152 sessions classified as "high-error-deep" (long sessions with >15% error
  rate) --- these are where the most time is wasted.

### First-prompt intent

- test/verify (1,104) is the #1 opener --- "run tests", "check this"
- create/implement (253) and fix/debug (94) are the main "do work" intents
- warmup/greeting (283) --- context-loading sessions

## Recommendations

### 1. Pre-flight file validation hook

A `pre_tool_use` hook on Edit/Write/MultiEdit that verifies the target file
exists and has been read in the current context. Eliminates ~640 errors (write-
without-read + edit-match failures).

### 2. Auto-test on edit

Since "test/verify" is the most common first prompt, a hook that auto-runs the
project's test suite after file edits would save many manual turns. Could be
project-specific via CLAUDE.md (e.g. `mix test`, `pytest`, `npm test`).

### 3. Path existence guard

Before Read/Edit/Write, verify the path exists. Could suggest corrections via
fuzzy matching (e.g. "did you mean `src/foo.ex`?"). Eliminates 823 "not found"
errors.

### 4. Smarter Bash timeout defaults

The default 2-minute timeout doesn't match workloads like `mix test` or long
builds. Project-specific timeout profiles (build commands get 5 min, git
commands get 30s) would eliminate 228 timeout errors.

### 5. Encourage parallel tool calls

With 0% parallelism, there's massive headroom. Stronger instructions or examples
in CLAUDE.md showing how to batch independent reads/greps/globs could cut
latency on common multi-file exploration patterns.

### 6. Error rate circuit breaker

For sessions where the error rate climbs above ~15%, prompt the agent to pause
and re-read the relevant files before continuing. The 152 high-error-deep
sessions suggest the agent sometimes spirals.

### 7. Sub-agent guardrails

The `mcp__acp__*` tools follow the same error patterns as main tools. Any guards
should apply uniformly to delegated work.
