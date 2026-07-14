---
id: TASK-024
title: Contribute extempore language support upstream to lumis
status: To Do
assignee: []
created_date: '2026-07-13 22:54'
updated_date: '2026-07-14 00:42'
labels:
  - upstream
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
lumis (https://lumis.sh) now powers ts-cat / yazi previews and nb show highlighting, but has no extempore parser, so .xtm files are mapped to scheme as an approximation. lumis's CONTRIBUTING.md documents an add-a-language recipe (languages.toml entry, feature-flag wiring, vendored parser build hook, queries, sample file, conformance tests), its architecture already includes niche languages behind per-language feature flags, and per-language query sources mean extempore's queries can come straight from extemporelang/tree-sitter-extempore (which already ships queries/highlights.scm). Ben authored the grammar, which answers the long-term maintenance question. Deferred until the mdx feasibility work lands and the maintainer's appetite is known.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 PR against leandrocp/lumis adds extempore per the documented recipe (manifest entry with pinned rev, feature flags, vendored parser wiring, queries, sample .xtm file)
- [ ] #2 cargo test --all-features and mise run test-conformance pass in the lumis repo
- [ ] #3 lumis highlight file.xtm renders xtlang-aware highlighting (bind-func etc.), verified locally before the PR is opened
- [ ] #4 once released, the scheme fallback mappings for .xtm in bin/ts-cat and nb/lumis-highlight.nb-plugin are removed
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Grammar freshened 2026-07-14 (rev cd9f1b5): s7 reader-model fixes (string escapes, signed radix literals, sharp_constant fallback, comments between quote mark and datum, scanner brace fix) plus queries/indents.scm added 2026-06-25. The lumis PR should pin rev cd9f1b5 or later. Related: MDX feasibility issue filed at https://github.com/leandrocp/lumis/issues/1011 with prototype branch benswift/lumis@mdx-prototype — the maintainer's response there will also signal appetite for the extempore addition, and the prototype's languages.toml/queries/feature-flag wiring is the template to follow.
<!-- SECTION:NOTES:END -->
