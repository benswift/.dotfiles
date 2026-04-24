---
id: TASK-012
title: working urlview/urlscan for neomutt
status: Done
assignee: []
created_date: '2025-10-09 22:41'
updated_date: '2026-04-24 01:59'
labels: []
dependencies: []
---

I've tried a few times, using both urlview (doesn't seem to work with
base64-encoded messages and I can't get neomutt to decode before piping?) and
urlscan (as per
<https://hunden.linuxkompis.se/2021/05/22/better-url-management-in-neomutt-with-urlview.html>,
but there are issues with it being run non-interactively I think?).

Anyway it's not a huge issue but it'd be good to fix at some point.

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
urlscan integrated into neomutt via commit 7979deb (2026-01-28). Active bindings: macro ,b extracts URLs in index/pager/attach modes.
<!-- SECTION:NOTES:END -->
