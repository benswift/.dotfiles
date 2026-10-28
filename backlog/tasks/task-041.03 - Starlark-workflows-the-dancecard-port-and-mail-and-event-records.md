---
id: TASK-041.03
title: Starlark workflows, the dancecard port, and mail and event records
status: Done
assignee: []
created_date: '2026-10-28T22:19:00Z'
updated_date: '2026-10-28T23:02:00Z'
labels:
  - pkb
  - wiz
dependencies:
  - TASK-041.02
parent_task_id: TASK-041
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Step 3 of TASK-041's order of work (see 'What leaves wiz's Go code'). Nothing in steps 4 to 6 depends on it: if it slips, the Go dancecard keeps serving lx.

- Starlark host (go.starlark.net) with the ctx API, CEL only through ctx.query, and golden-output tests against a fixture vault
- dancecard as an lx Starlark workflow, with vault_test.go's coverage ported to golden tests before dancecard.go is deleted
- mail-export does the mail cleaning and writes mail records (_types/mail.md)
- calendar-archive gains --calendar and an lxconvenor entry, writes event records with the vault's field names, links attendees to people; it runs as a command workflow in each vault (home on weddle, lx on watson, where watson-run runs mu index after its mbsync pull)
- delete calendar.go, admin/tools/calendar-export, _generated/calendar.md, home's data/calendars/*.jsonl and the calendar-archive timer
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 Starlark workflows run on macOS arm64, Linux amd64 and Windows amd64 with nothing installed and have no exec, filesystem or network access and no way to invoke a command workflow; anyone may add one, while command workflows, _types/ and mdbase.yaml are admin-only and the guard enforces the split
- [x] #2 The lx dancecard is a Starlark workflow whose output matches the Go version on the same vault and date, covered by golden-output tests in the engine's Go suite; dancecard.go and calendar.go are deleted
- [x] #3 The engine contains no LX-specific types, folders or logic; a vault's types, rules and workflows come only from its own files
- [x] #4 The lx vault carries an event type, with field names matching delivery's where they overlap, filled by calendar-archive as a command workflow (verified on watson against a scratch clone), with attendees linked to people where an email matches; a re-run against an unchanged calendar touches no file
- [x] #5 The lx dancecard's coming-up and mail sections come from event and mail records
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Engine side done (Starlark host, golden tests, dancecard.go and calendar.go deleted). lx: _types/mail.md and event.md, mail-export writes summary, dancecard and unrecorded-deliveries Starlark workflows, calendar command workflow; verified on watson against a scratch clone, the event lines matching calendar-export's line for line. Left for the switch (TASK-041.07): deleting admin/tools/calendar-export and _generated/calendar.md and watson-run's calendar step, since the v0.9 dancecard Cole and Rob run reads them. Left for the home migration (TASK-041.05): home's event type, its calendar workflow, the --seed import of data/calendars/*.jsonl and the calendar-archive timer's removal.
<!-- SECTION:NOTES:END -->
