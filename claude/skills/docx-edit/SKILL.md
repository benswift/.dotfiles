---
name: docx-edit
description:
  Programmatically edit or patch a Microsoft Word .docx in place with
  python-docx, preserving its formatting and existing edits, as a safe additive
  sync rather than regenerating the file. Use when porting content into an
  authoritative or hand-edited .docx --- a submission, a fill-in form, a
  reviewer's working copy, an OneDrive/SharePoint document a collaborator may
  have marked up --- where you must keep italics/bold, form fields and any prior
  changes intact. Triggers include "edit the docx", "patch a Word doc", "sync
  into the Word file", "update the .docx without overwriting", "tick a checkbox
  in the form", and "preserve the track changes / formatting".
---

# Editing a .docx in place

The job: change the content of a Word document with code, without losing what is
already in it. The naive move --- regenerate the whole file from your source ---
is wrong whenever the .docx is itself authoritative: someone may have edited it
in place (track changes, reviewer comments, a manually corrected number), or it
carries form scaffolding you can't reproduce. So you **patch it**: apply only
the genuine deltas, at the run level, and prove afterwards that nothing else
moved.

This skill is general. The reusable helpers live in `references/docx_helpers.py`
(import them, or copy the few you need).

## When this applies

- A `.docx` is the thing being submitted or shared, and your text lives
  somewhere else (Markdown, Typst, a database) that you're syncing _from_.
- The document may have drifted ahead of your source --- a collaborator edited
  it, or an earlier sync already landed some changes.
- You need to set form fields (the grey FORMCHECKBOX boxes), not just prose.

If instead you fully own the file and nobody edits it but your generator, just
regenerate it --- this ceremony is for the authoritative-copy case.

## The core idea: edit runs, not paragraphs

In OOXML a paragraph (`<w:p>`) is a list of runs (`<w:r>`), and **formatting
lives on the run** --- an italic work-title or a bold phrase is its own run.
Rewriting `paragraph.text` collapses every run into one and throws the
italics/bold away. So:

- to change wording, rebuild the paragraph's runs as
  `[(text, italic, bold), ...]` and write them back with `set_runs` (it keeps
  the paragraph's `<w:pPr>` --- style, spacing, numbering);
- to drop a sentence, pass the runs that remain (or `[]` to blank the
  paragraph), rather than splicing strings;
- match new runs' font/size to the surrounding runs --- inspect an existing
  run's XML once if unsure (Word's size unit is half-points, so 10pt is `20`).

## The safe-sync workflow

Follow this every time; each step has caught a real failure.

1. **Confirm the editor is closed.** `pgrep -x "Microsoft Word"` (or the
   relevant app). An open client holds a lock and will overwrite your change on
   its next save, or refuse to sync it back. Abort if it's open.
2. **Work on a copy, keep a timestamped backup.** Never edit the live file
   directly. `cp live.docx backup-$(date +%Y%m%d-%H%M%S).docx` and edit a
   separate `working.docx`.
3. **Establish the baseline.** If the live file is supposed to match a prior
   sync, check it (`md5`); if it doesn't, it has drifted --- diff the live
   paragraphs against your source and reconcile by hand before patching, because
   the drift may be edits you must preserve. Don't trust a git diff of your
   _source_ to tell you what the _doc_ needs.
4. **Assert before you edit (fail fast).** Locate each target by stable text
   (`find_para`) and confirm the current content is what you expect before
   changing it. A moved anchor should raise, not silently edit the wrong spot.
5. **Apply the deltas** with `set_runs` / `set_checked`. Keep checkbox and form
   edits **additive** --- tick what you mean to, never blanket-clear.
6. **Diff to prove minimality.** Run `diff_paragraphs(backup, working)`: the
   only differences should be the paragraphs you intended. Same paragraph count,
   nothing stray. This is the step that catches an edit landing in the wrong
   place.
7. **Verify formatting.** Re-open `working.docx` and confirm the text _and_ the
   italic/bold spans match your intent (compare merged-by-flag spans, not raw
   run boundaries --- python-docx may split a span across runs).
8. **Promote and confirm.** Copy `working.docx` over the live file, then `md5`
   both to confirm the bytes match. Leave the backup in place.

## python-docx gotchas

- **Round-trips shrink the file.** python-docx drops parts it doesn't model
  (custom XML, unused styles), so a save can take the file from e.g. 106KB to
  99KB. The _content_ is preserved, but keep the backup --- and know the size
  drop is expected, not corruption.
- **Whitespace.** New runs need `<w:t xml:space="preserve">` or leading/trailing
  spaces vanish and words fuse. `make_run` does this.
- **Style detection is uneven.** Don't assume all headings share a style or all
  body paragraphs do --- a doc often mixes `Heading 2`, `Normal` and custom
  styles for visually identical text. A pass keyed on
  `style.startswith("Heading")` will misfire; anchor on explicit text instead.
- **Legacy checkboxes** are `<w:checkBox>` inside `<w:ffData>` (FORMCHECKBOX),
  not content controls or glyphs. "Checked" is usually the presence of a
  `<w:checked/>` child, but some forms keep a persistent
  `<w:checked w:val="0|1"/>` and toggle the value --- `is_checked` /
  `set_checked` handle both, but inspect your doc once to see which it uses.
- **Cloud-synced folders (OneDrive/SharePoint).** The local file is the sync
  client's copy. After you overwrite it the client uploads on its own schedule;
  if you need the change visible server-side promptly, give it a moment or
  trigger a sync. And because a collaborator can edit server-side, re-check the
  baseline (step 3) every time --- the folder is authoritative, so never
  blind-overwrite it.

## Minimal example

```python
# uv run --with python-docx python sync.py
import shutil, sys
sys.path.insert(0, "references")
from docx import Document
from docx_helpers import find_para, set_runs, set_checked, checkboxes_in, diff_paragraphs

shutil.copyfile("live.docx", "backup.docx")
shutil.copyfile("live.docx", "working.docx")

doc = Document("working.docx")
i = find_para(doc, "2.2 Education")              # stable anchor
target = doc.paragraphs[i + 1]
assert "accredited professional development" in target.text   # assert before edit
set_runs(target, [
    ("It is ", False, False),
    ("peer-reviewed pedagogy", False, True),     # bold span kept as its own run
    (" and accredited PD for teachers.", False, False),
], size_half_pt=20)
doc.save("working.docx")

for d in diff_paragraphs("backup.docx", "working.docx"):
    print(d)                                     # expect only the para you touched
```

Then verify, copy `working.docx` over `live.docx`, and `md5` both.
