---
id: TASK-012
title: working urlview/urlscan for neomutt
status: Done
assignee: []
created_date: "2025-10-09 22:41"
updated_date: "2026-04-24 01:59"
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

urlscan integrated into neomutt via commit 7979deb (2026-01-28). Active
bindings: macro ,b extracts URLs in index/pager/attach modes.

Replaced urlscan with the `mail-urls` command (mail/utils package) on
2026-07-08: urlscan regex-scraped neomutt's decoded text and missed most links
in HTML mail. `mail-urls` clears pipe_decode, parses the raw MIME/HTML with an
actual parser (real href/src/anchor-text), dedupes, and presents an fzf picker
(enter=open, tab=multi-select, ctrl-y=copy). Verified end-to-end against a real
HTML newsletter (16 links extracted vs 0 from urlscan).
<!-- SECTION:NOTES:END -->
