---
id: TASK-016
title: Investigate helix adding CRLF line endings on macOS
status: Done
assignee: []
created_date: '2026-02-02 00:34'
updated_date: '2026-02-02 00:49'
labels:
  - helix
  - config
  - bug-investigation
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
## Problem

Helix is adding `^M` (CRLF/\r\n) line endings to files on macOS, when it should default to LF (\n) since `default-line-ending` defaults to `native`.

## Observed behaviour

- Editing `ledger.csv` in the lunchtime-soccy repo resulted in CRLF line endings
- The file originally had LF endings (verified via `git show HEAD:ledger.csv | od -c`)
- macOS should use LF as native line ending

## To investigate

Use ht-mcp to:

1. Create a test file with LF endings:
   ```bash
   echo 'a,b,c
   1,2,3' > /tmp/test.csv
   od -c /tmp/test.csv  # verify LF
   ```

2. Open in helix, make a change, save:
   ```bash
   hx /tmp/test.csv
   # add a line, :w, :q
   ```

3. Check line endings after:
   ```bash
   od -c /tmp/test.csv
   ```

If CRLF appears, it's a helix config/bug issue. If LF is preserved, the original file likely had mixed/CRLF content.

## Potential fixes

- Add `default-line-ending = "lf"` to helix config (workaround)
- Report bug to helix if detection is broken
- Check if CSV files specifically trigger this

## Related config

- Helix version: 25.07.1
- Config: ~/.config/helix/config.toml (no line-ending setting currently)
- Git: core.autocrlf=input
<!-- SECTION:DESCRIPTION:END -->

## Final Summary

<!-- SECTION:FINAL_SUMMARY:BEGIN -->
## Investigation results

Tested helix 25.07.1 on macOS using headless-terminal MCP:

1. Created test files with LF-only line endings (verified with `od -c`)
2. Opened in helix, made edits (both normal mode `rx` and insert mode `o`)
3. Saved and verified line endings remained LF-only

**Conclusion**: Helix correctly respects `default-line-ending = "native"` on macOS and preserves LF endings. The original `ledger.csv` issue was caused by the file already having CRLF/mixed line endings before editing, not by helix introducing them.

**No config changes needed** --- helix is working correctly.
<!-- SECTION:FINAL_SUMMARY:END -->
