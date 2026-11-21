---
id: TASK-042
title: Tree-sitter grammar for ABC music notation, for Helix
status: To Do
assignee: []
created_date: '2026-11-21T07:36:00Z'
labels:
  - helix
  - tree-sitter
dependencies: []
references:
  - https://abcnotation.com/wiki/abc:standard:v2.1
  - https://web.mit.edu/6.031/www/sp18/projects/karaoke/spec/abc_subset_bnf.txt
  - https://github.com/AntoineBalaine/tree-sitter-abc
  - https://github.com/paulrosen/abcjs
  - https://docs.helix-editor.com/guides/adding_languages.html
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Helix 25.07 has no ABC support (`hx --health abc`: language not found), and the trumpet parts in ~/projects/chord-charts (`songs/*.abc`) are now edited by hand. None of the existing grammars is usable: mantou132/tree-sitter-abc is a tutorial language under ABC's name, AlexHarter/tree-sitter-abc holds only a licence, and AntoineBalaine/tree-sitter-abc (2022, no licence, empty queries) fails on everyday input such as a space before a bar line (1--14 ERROR nodes on three of the four chord-charts parts). So write our own, as `benswift/tree-sitter-abc` alongside `benswift/tree-sitter-mutt-compose`, pinned by rev in `helix/languages.toml` the same way.

ABC has no complete formal grammar for 2.1: the standard is detailed prose, Henrik Norbeck's 1997 BNF covers 1.6 (now only in web archives), and MIT 6.005 published BNF subsets of 1.6 and 2.1. Use the 2.1 prose as the authority and the BNFs and existing parsers (abcjs, the abc-parser crate's PEG, AntoineBalaine's grammar.js) as a starting point. Be tolerant of whitespace: the files are hand-typed, and Verovio and abcjs accept loose spacing.

Creating the public GitHub repo is outward-facing: ask Ben before `gh repo create`.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Parses the file header and tunes: X:, T:, M:, L:, K:, Q: and other info fields, inline fields like [K:D], %% directives and % comments
- [ ] #2 Parses the music: notes with accidentals, octave marks and lengths (A3, B/2, c>d), rests, ties, slurs, tuplets, chords in [], grace notes, decorations, and quoted chord symbols and annotations
- [ ] #3 Parses bar lines, repeats and endings: |, ||, |], [|, |:, :|, ::, |1, :|2, [1
- [ ] #4 Parses w: lyric lines
- [ ] #5 Every .abc in chord-charts songs/ and tests/fixtures/ parses with no ERROR or MISSING nodes
- [ ] #6 tree-sitter test corpus covers each construct above, and passes
- [ ] #7 Generated src/parser.c is committed (Helix builds from it), with an MIT licence and a README naming the sources
- [ ] #8 helix/languages.toml has an abc language (file-type abc, comment-token %) and a pinned grammar; hx --health abc reports the grammar and highlight queries
- [ ] #9 Highlight queries colour fields, notes, accidentals, bar lines and repeats, comments and chord symbols distinctly in Ben's Helix theme
<!-- AC:END -->
