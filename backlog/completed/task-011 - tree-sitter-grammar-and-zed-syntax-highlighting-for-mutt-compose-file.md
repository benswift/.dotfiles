---
id: task-011
title: tree-sitter grammar and zed syntax highlighting for mutt compose file
status: Done
assignee: []
created_date: "2025-09-02 06:32"
labels: []
dependencies: []
---

## Description

Part of the mutt compose workflow is to create a file in which to write the
email. Here's a real example of one (note the `ACTUAL REPLY GOES HERE` text):

```
From: Ben Swift <ben.swift@anu.edu.au>
To: SOCY – Admin <admin.cybernetics@anu.edu.au>
Cc: Ushini Attanayake <ushini.attanayake@anu.edu.au>
Bcc:
Subject: Re: PhD Final Presentation Booking
Message-ID: <gwwxwdznyauzei4qhexk4q2ezqcu5dtw26ynxiur4ktfxvocli@lpoqflwncgek>
Reply-To:
In-Reply-To: <SY4P282MB351791F7CD6D7FE4266087D3C806A@SY4P282MB3517.AUSP282.PROD.OUTLOOK.COM>

ACTUAL REPLY GOES HERE

On Tue, 02 Sep 2025 at 04:24 PM +1000, SOCY – Admin wrote:
>Hi Ben, 
>
>Sorry for the oversight, haven’t had time to reply to emails in that
>inbox since last Friday. Yes the innovation space is available, will
>send calendar invite shortly
>
>Warm regards,
>
>Danni Liu
>School Administrator
>
>Operational Excellence Team
>School of Cybernetics
>ANU College of Systems and Society
>
>Level 3 Birch Building, 35 Science Road
>The Australian National University
>Canberra ACT 2600
>
>+61 2 6125 8121
>
>Admin.Cybernetics@anu.edu.au
>
>Danni.Liu@anu.edu.au
>
> 
>
>TEQSA Provider ID: PRV12002 (Australian University) | CRICOS Provider
>Code: 00120C
>
>------------------------------------------------------------------------
>
>From: Ben Swift <ben.swift@anu.edu.au>
>Sent: Tuesday, September 2, 2025 16:06
>To: SOCY – Admin <admin.cybernetics@anu.edu.au>
>Cc: Ushini Attanayake <ushini.attanayake@anu.edu.au>
>Subject: Fwd: Re: PhD Final Presentation Booking
>
> 
>
>Hey Danni,
>See below, my PhD student Ushini is trying to book the innovation space
>for her PhD final presentation,
>hasn't heard anything from the innovationspace.cecc@anu.edu.au address.
>Do you know where
>she should be directing this request?
>Cheers
>Ben
```

However, my zed text editor doesn't understand the structure of that file. I'd
like to develop a custom zed extension that:

1. contained a tree sitter grammar to recognise the various parts of the above
   email (headers, body content, "reply stuff" down the bottom, etc.)
2. used syntax highlighting to visually indicate which bits were which
3. (optional) considered the body to be markdown (rather than plain text) for
   syntax highlighting purposes

It should apply this grammar to all such files which end up in the neomutt cache
directory.
