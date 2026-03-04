---
name: pkb
description:
  Manages Ben's personal knowledge base --- a hybrid human+Claude journal and
  note system built on nb, with email access via mu/neomutt. Use when working
  with notes, journaling, capturing ideas, managing people/project/topic notes,
  searching the knowledge base, reading or sending email, or any task involving
  Ben's personal information system. Triggers include "journal", "note", "search
  notes", "check email", "send email", "what are my projects", "add a todo", or
  any reference to the notebook, knowledge base, or email.
---

# Personal knowledge base

Read `~/.nb/home/CLAUDE.md` for notebook structure, templates, conventions, nb
CLI reference, and Claude's responsibilities. That file is the single source of
truth --- follow the instructions therein.

```bash
cat ~/.nb/home/CLAUDE.md
```

## Email

For email search (mu find/view) and sending (neomutt), see `email.md` in this
skill directory.

Key points:

- three accounts: personal (Fastmail), anu (Office365), phdconvenor (delegated)
- default to `mu find` + `mu view` for searching
- use neomutt via headless-terminal only when interactive browsing or sending is
  needed
