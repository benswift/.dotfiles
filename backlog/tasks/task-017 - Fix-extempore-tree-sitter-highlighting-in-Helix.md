---
id: TASK-017
title: Fix extempore tree-sitter highlighting in Helix
status: In Progress
assignee: []
created_date: '2026-02-24 16:00'
updated_date: '2026-02-24 03:50'
labels:
  - helix
  - tree-sitter
  - extempore
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Extempore `.xtm` files get partial syntax highlighting in Helix --- some symbols
(e.g. `println`, `a`) are highlighted correctly, but others (`bind-func`,
`define`, `lambda`) appear in the default text colour despite matching identical
query patterns.

## Root cause: tree-house query engine mismatch

Helix 25.07.1 replaced its tree-sitter Rust bindings with
[tree-house](https://github.com/helix-editor/tree-house), a from-scratch
rewrite. The extempore grammar loads (ABI 14/15 both accepted), and `hx --health
extempore` reports highlight queries OK. However, tree-house's query matching
behaves differently from the standard tree-sitter CLI for certain `symbol` nodes.

### What works

- `tree-sitter parse` produces the correct parse tree --- `bind-func`, `define`,
  `lambda`, `println` are all `symbol` nodes as first named children of `list`
  nodes
- `tree-sitter highlight` correctly highlights all symbols
- `tree-sitter query '(list . (symbol) @function)'` matches all 6 first-in-list
  symbols in the test file
- All 170 grammar corpus tests pass

### What doesn't work in Helix

- `(symbol) @function` (no anchor, no predicate) highlights `println` and `a`
  but NOT `bind-func`, `define`, or `lambda`
- `(_) @function` (wildcard) DOES highlight everything including `bind-func` ---
  so those nodes exist, they're just not being recognised as `symbol` type by
  tree-house
- Tested with both ABI 14 (original parser.c) and ABI 15 (regenerated with
  tree-sitter 0.26.5) --- same behaviour
- Grammar binary `.so` was verified identical sources (parser.c, scanner.c)
  between tree-sitter CLI and Helix

### Diagnosis so far

The issue is NOT:
- Pattern priority ordering (tested with single-pattern highlights.scm)
- `#match?` predicates (tested without any predicates)
- `.` anchor semantics (tested without anchor)
- ABI version mismatch (tested both 14 and 15)
- Grammar differences (parser.c identical between CLI and Helix)
- Query file location (verified and synced across all locations)
- Stale grammar binary (rebuilt multiple times)

The issue IS:
- tree-house's query engine not matching `(symbol)` for certain symbol nodes that
  the standard tree-sitter CLI matches correctly
- Possibly a bug in tree-house, or a subtle ABI/node-type-table interpretation
  difference

## Capture name mapping

Separately, Helix's gruvbox theme requires specific capture names. `@number`
doesn't work --- must use `@constant.numeric`. The production highlights.scm
needs a full audit of capture names against Helix's capture hierarchy. Known
mappings for gruvbox:

| Capture             | Colour         |
|---------------------|----------------|
| `@keyword`          | red            |
| `@function`         | green          |
| `@function.builtin` | yellow         |
| `@type`             | yellow         |
| `@string`           | green          |
| `@comment`          | gray italic    |
| `@constant.numeric` | purple         |
| `@constant.builtin` | purple         |
| `@punctuation`      | orange         |
| `@operator`         | purple         |
| `@variable`         | base/default!  |

Note: `@variable.definition` resolves to `@variable` which is the base
foreground colour --- effectively invisible. Need a different capture for
definition names.

## Relevant paths

- Grammar repo: `/Users/ben/Code/extemporelang/tree-sitter-extempore/`
- Grammar repo (GitHub): `https://github.com/extemporelang/tree-sitter-extempore`
- Helix query files (user override): `~/.config/helix/runtime/queries/extempore/`
- Helix grammar sources: `~/.config/helix/runtime/grammars/sources/extempore/`
- Helix grammar binary: `~/.config/helix/runtime/grammars/extempore.so`
- Helix languages.toml: `~/.config/helix/languages.toml`
- Dotfiles copy: `~/.dotfiles/helix/runtime/queries/extempore/highlights.scm`
- Grammar rev in languages.toml: `91c8946ae8886e5798bfe9c4beae2b6c69fde7b5`

There is an unpushed commit `34c03d1` in the local grammar repo (fixes highlight
priority ordering, adds compound type corpus tests). Push this before updating
the rev in languages.toml.

## Current file state

- `~/.config/helix/runtime/queries/extempore/highlights.scm` --- restored to
  full production version (reordered, catch-all first, specific patterns last)
- `~/.dotfiles/helix/runtime/queries/extempore/highlights.scm` --- has the same
  production version
- Grammar binary was last rebuilt with ABI 15 parser
- Backup files at `.bak` paths can be deleted
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
<!-- SECTION:ACCEPTANCE_CRITERIA:BEGIN -->
- [ ] #1 `bind-func`, `define`, `lambda` highlighted as keywords (red) in Helix
- [ ] #2 Function names after `bind-func` highlighted as functions (green) in Helix
- [ ] #3 Type annotations highlighted as types (yellow) in Helix
- [ ] #4 All capture names in highlights.scm verified against Helix theme hierarchy
- [ ] #5 Grammar commit pushed to GitHub and rev updated in languages.toml
<!-- SECTION:ACCEPTANCE_CRITERIA:END -->
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Investigation summary

### Workaround applied

Rewrote `highlights.scm` to use `(_)` wildcard instead of `(symbol)` for all
first-in-list pattern matching. This is the standard tree-sitter approach when
matching multiple possible node types. All `(symbol) @capture (#match? ...)`
patterns replaced with `(_) @capture (#match? ...)`.

Additional fixes in the rewrite:
- `@number` → `@constant.numeric` (gruvbox compatibility)
- `@variable.definition` → `@label` (avoid invisible base colour)
- Pattern ordering: catch-all `@function.call` first (lowest priority), specific
  patterns last (highest priority)

Tree-sitter CLI confirms all captures are correct with the new queries.

### Root cause analysis

The issue is a discrepancy between tree-sitter C library versions:
- **tree-house-bindings 0.2.3** vendors tree-sitter C library from the **0.25.x
  era** (api.h deprecates functions "to be removed in 0.26")
- **tree-sitter CLI** uses version **0.26.5**
- Both support `TREE_SITTER_LANGUAGE_VERSION 15`

The query matching code in `query.c` (line 3854) does a direct symbol ID
comparison: `node_does_match = symbol == step->symbol`. Bug fixes between 0.25.x
and 0.26.5 in this code path could explain why `(symbol)` matches all nodes in
the CLI but fails for some nodes in tree-house.

The C++ highlighting regression in Helix issue #14139 (also 25.07.1) is
potentially related --- same tree-house version.

### Verified facts
- `bind-func` IS a `(symbol)` node (confirmed via `:tree-sitter-subtree` in Helix)
- External scanner returns `false` for plain symbols (no `:` or `{`), so parser
  falls back to internal lexer which produces `(symbol)`
- `ts_symbol_map` is an identity mapping (no aliasing)
- Grammar health check passes: `hx --health extempore` shows ✓ for parser and
  highlights
- No existing Helix or tree-house issues match this specific bug

### Needs visual verification

Headless terminal snapshots cannot show popup content (ANSI colours stripped).
The `:tree-sitter-highlight-name` popup DID appear for `bind-func` (vs "nothing
selected" for unhighlighted positions), suggesting the `(_)` workaround is
working, but definitive confirmation requires opening a `.xtm` file in a real
terminal.

### For tree-house bug report

Minimal reproduction:
1. Install extempore grammar: `hx --grammar build` (or use existing)
2. Create test query with `(list . (symbol) @keyword (#match? @keyword "^bind-func$"))`
3. Open test file with `(bind-func foo 42)`
4. Run `:tree-sitter-highlight-name` on `bind-func` → no highlight
5. Change query to use `(_)` instead of `(symbol)` → highlight appears
6. Compare with `tree-sitter query` using same grammar and query → matches correctly
<!-- SECTION:NOTES:END -->
