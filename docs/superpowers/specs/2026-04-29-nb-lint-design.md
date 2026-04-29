# nb lint: broken-wikilink linter

**Date:** 2026-04-29
**Status:** design

## Goal

Catch broken `[[folder/target]]` wikilinks in nb notebooks before they
silently rot. Surface results in a way that's useful both interactively
(today) and from a future nightly reflection cron (planned).

## Scope

In:

- new nb plugin `lint.nb-plugin` shipped with the dotfiles
- a small symlink change in `create_symlinks.sh` that registers all
  `~/.dotfiles/nb/*.nb-plugin` files into `~/.nb/.plugins/`

Out:

- bare wikilinks like `[[mounts]]` (ambiguous; not what `~/.nb/home/CLAUDE.md`
  documents as the convention)
- broken-anchor detection inside link targets (`[[foo#missing-section]]`
  is treated the same as `[[foo]]`)
- auto-fixing or rename suggestions
- nightly cron orchestration (the cron is a separate piece of work; the
  linter is just one CLI it'll call)
- backwards-compatibility checks for the old `[[ideas/...]]` references
  (we already verified there are none)

## Components

### 1. `~/.dotfiles/nb/lint.nb-plugin`

Bash plugin following the convention of the existing `cpath`, `pdf`,
`backlog`, and `docx` plugins. Registers a `lint` subcommand on nb.

**Usage:**

```sh
nb lint               # default: report broken wikilinks in current notebook
nb lint --summary     # one-line "<N> broken wikilinks across <M> files"
nb lint <notebook>    # lint a specific notebook
```

Exit codes:

- `0` — no broken wikilinks
- `1` — one or more broken wikilinks
- `2` — invocation error (missing notebook, etc.)

### 2. `create_symlinks.sh` change

Add a section that symlinks each `~/.dotfiles/nb/*.nb-plugin` file into
`~/.nb/.plugins/<name>.nb-plugin`. nb's plugin loader treats anything in
`~/.nb/.plugins/` as installed, and follows symlinks, so this gives us
"edit in `~/.dotfiles/nb/`, see changes immediately" with no manual
`nb plugins install` step.

This also retroactively installs the existing 4 plugins on machines
where they aren't currently registered (verified: this Linux machine
shows `nb plugins` → "No plugins found" despite the dotfiles entries).

## Behaviour

### What counts as a wikilink

Any `[[target]]` where `target` matches the pattern
`<folder>/<slug-or-id>(#<anchor>)?`.

- `<folder>` — one or more characters, no slashes, no whitespace
- `<slug-or-id>` — one or more characters, no slashes, no whitespace
- `(#<anchor>)?` — optional, ignored for resolution

Wikilinks without a `/` are not checked (per design decision: bare links
are casual references, not navigable links per the documented convention).

### Resolution rules

For `[[<folder>/<target>]]`:

1. Strip any trailing `#<anchor>`.
2. If `<target>` is purely numeric, run `nb show <notebook>:<folder>/<target> --path`
   and treat exit code 0 as resolved, non-zero as broken. (Numeric ids drift
   as notes are added, moved, or deleted, so prefer file-based slugs in new
   wikilinks --- numeric resolution is supported only for legacy refs.)
3. Otherwise, resolved if **either** `<notebook-path>/<folder>/<target>.md` OR
   `<notebook-path>/<folder>/<target>` (literal, no extension appended) exists,
   case-sensitively. The literal-filename fallback lets wikilinks like
   `[[scripts/foo.py]]` resolve to non-`.md` files.

Notebook path comes from `_notebooks <name> --path` (or the current
notebook if no name given).

### What gets scanned

All `*.md` files under the notebook root, **except**:

- files inside `.git/`
- files inside `data/icloud/` (gitignored, intentionally invisible)
- the `.plugins` and `.archive` directories nb may create

### What gets ignored within a file

Wikilinks inside fenced code blocks (delimited by triple-backticks).
This avoids false positives in documentation files (notably
`~/.nb/home/CLAUDE.md`, which contains `[[folder/item-name]]` and
`[[projects/panic]]` as documentation examples). Implementation:
single-pass state machine that toggles in/out of a fence on each
`^` + 3-or-more backticks line.

Inline code (`` `[[foo/bar]]` ``) is **not** stripped — keeping the
implementation simple, and the false-positive rate from inline code is
expected to be near zero.

### Output format (default)

One line per broken link, rg-compatible:

```
people/jane-doe.md:14:5: [[projects/missing-project]] -- target not found
projects/comp4020.md:42:12: [[topics/llm-mental-models-old]] -- target not found
```

Format: `<relative-path>:<line>:<col>: <link-as-written> -- <reason>`

Path is relative to the notebook root. Column is the 1-indexed start of
the `[[`.

### Output format (`--summary`)

One line:

```
3 broken wikilinks across 2 files
```

Or, when clean:

```
0 broken wikilinks
```

## Edge cases

- **Case sensitivity:** strict. `[[projects/comp4020]]` does not resolve
  to `projects/COMP4020.md`. Catches genuine inconsistencies.
- **Anchors:** stripped before resolution. No validation that the anchor
  exists in the target file.
- **Numeric ids:** resolved via `nb show`. If the id has been reassigned
  (e.g., a note was deleted and the id reused for a different file),
  the link still resolves — accepted: the link will surface as wrong
  semantically but not as broken syntactically. Not in scope.
- **Notebooks other than `home`:** lint runs on the current notebook by
  default; an optional `<notebook>` argument selects another.
- **Multiple links on one line:** all reported, each with its own
  column number.
- **Links spanning lines:** not supported. Wikilinks are expected to be
  on a single line.
- **Empty target (`[[/]]`, `[[foo/]]`, `[[/bar]]`):** matched by the pattern
  and reported as broken (these are typos in practice; flagging them is more
  useful than silent ignore).

## Testing

A `bats` test file at `~/.dotfiles/test/nb-lint.bats` exercising:

1. clean notebook → exit 0, no output
2. one broken non-numeric link → exit 1, correct path:line:col
3. one broken numeric link → exit 1, correct reporting
4. wikilink inside fenced code block → not flagged
5. wikilink with anchor → resolved by the part before `#`
6. case-mismatched link → flagged as broken
7. bare `[[foo]]` link → ignored
8. `--summary` mode formatting (clean and dirty)
9. multiple links on one line, mix of broken and resolved → only
   broken ones reported, with correct columns

Tests use a fixture notebook created in a temp dir, populated with
known-content notes via `mkdir`/`cat` (no need for nb itself in the
fixture beyond what's available system-wide for id resolution).

If `bats` isn't already available, the install step is a one-liner
(`apt`/`brew`) — but the test file should also be runnable directly as
bash (e.g., a small `assert` helper), so a missing `bats` doesn't
block use.

## Future work (out of scope here)

- a `--fix` mode that proposes renames based on closest-match
- the nightly reflection cron that calls this and other tools
- broken-anchor detection
- bare-wikilink resolution (would require a global filename index)
- a JSON output mode for the reflection cron (defer until it exists
  and we know what it actually needs)

## Open questions

None at design time. The implementation plan can proceed.
