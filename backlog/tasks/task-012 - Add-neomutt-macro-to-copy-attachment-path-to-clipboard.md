---
id: task-012
title: Add neomutt macro to copy attachment path to clipboard
status: Done
assignee: []
created_date: "2025-09-15 11:13"
labels: []
dependencies: []
---

This is WONTFIX; it was too tricky. Need a better solution.

## Description

Add a macro to neomutt config that copies the full filesystem path of an
attachment to the system clipboard (macOS pbcopy). Use the print_command trick
with %s expando to get the temporary file path.

## Implementation

Add to `~/.config/neomutt/neomuttrc`:

```
macro attach C "<enter-command>set print_command='echo -n %s | pbcopy'<enter><print-entry><enter-command>set print_command=lpr<enter>" "Copy path to clipboard"
```

This works by:

1. Temporarily setting the print command to copy the path to clipboard
2. "Printing" the attachment (which runs our copy command with %s replaced by
   the temp file path)
3. Resetting the print command back to the default

Bind to `C` key in attachment view.
