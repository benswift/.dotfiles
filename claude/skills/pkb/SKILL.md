---
name: pkb
description:
  Manages Ben's personal knowledge base --- a hybrid human+Claude journal and
  note system built on nb, with email access via mu/neomutt. Use when working
  with notes, journaling, capturing ideas, managing people/project/topic notes,
  searching the knowledge base, reading or sending email, or any task involving
  Ben's personal information system. Subsumes the nb-notebook-manager and
  email-manager skills. Triggers include "journal", "note", "search notes",
  "check email", "send email", "what are my projects", "add a todo", or any
  reference to the notebook, knowledge base, or email.
---

# Personal knowledge base

Ben's PKB is a hybrid human+Claude system. Ben journals conversationally; Claude
maintains structured notes, tracks ideas, and keeps the knowledge base current.
Storage is an [nb](https://xwmx.github.io/nb/) notebook at `~/.nb/home/`. Email
is read-only context via mu, with sending via neomutt.

## First steps

Read the notebook's CLAUDE.md for structure, templates, and conventions:

```bash
cat ~/.nb/home/CLAUDE.md
```

This file defines the folder layout, YAML frontmatter templates per folder,
tag format (`tags: #tag1 #tag2`), cross-linking conventions, daily note usage,
and session startup queries.

## Building context

When starting a session or when context would help, run the session startup
queries from CLAUDE.md to build a picture of the current state. At minimum:

```bash
nb list projects/ --no-color
nb list --type todo --no-color --limit 10
nb list --type daily --no-color --limit 3
```

## Responsibilities

When Ben journals or discusses work:

- **capture**: create or update notes for new people, topics, ideas, projects
- **connect**: add `[[folder/item-name]]` wiki-links to related notes
- **track**: create todos for action items that emerge from conversation
- **summarise**: update project and people notes with new developments
- **daily**: append journal entries and session summaries to the daily note

Only create notes when Ben mentions something substantive worth remembering.

## nb reference

For nb CLI commands (search, list, show, add, edit, delete, move, bookmarks,
todos), see `references/nb.md`.

Key points:

- all nb operations are non-interactive --- never use flags that open an editor
  or pager
- use `nb show <id> --path` to get file paths, then use Read/Edit tools
- never use `nb show` output directly for editing (contains ANSI colour codes)
- tags use inline `#hashtag` format: `tags: #tag1 #tag2`
- `NB_AUTO_SYNC` is off; daily scheduled sync handles it

## Email reference

For email search (mu find/view) and sending (neomutt), see `references/email.md`.

Key points:

- three accounts: personal (Fastmail), anu (Office365), phdconvenor (delegated)
- default to `mu find` + `mu view` for searching
- use neomutt via headless-terminal only when interactive browsing or sending is
  needed
