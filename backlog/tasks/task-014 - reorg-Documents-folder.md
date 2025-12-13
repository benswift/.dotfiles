---
id: task-014
title: reorg ~/Documents/ folder
status: To Do
priority: medium
assignee: []
created_date: "2025-12-05 06:30"
labels: [organisation, cleanup]
dependencies: []
---

Reorganise ~/Documents/ to create a clearer separation between code, research,
teaching, and general documents.

## Current state

- 390 git repos in ~/Documents/ (many archival, dating back to 2009)
- 43 git repos in ~/Code/ (well-organised by language)
- 135GB in Documents, 29GB in Code
- Key categories: teaching (179 repos, 62GB), research (136 repos, 45GB), edex
  (45 repos, 18GB)

## Target structure

```
~/Code/              # Active coding projects (by language) - keep as-is
~/Documents/         # Non-code documents, app data, media
~/Documents/research # Academic research (reorganised)
~/Documents/teaching # Course materials (reorganised)
```

## Implementation plan

### Recommended: run the idempotent script

This plan is encoded in an idempotent, collision-safe script (defaults to
dry-run and refuses to overwrite anything):

```bash
bash backlog/tasks/task-014-reorg-Documents-folder.sh
bash backlog/tasks/task-014-reorg-Documents-folder.sh --apply
```

### Key changes

1. Software projects from edex move to ~/Code/elixir/ and ~/Code/js/
   - fair_dinkum renamed to fair_dinkum_game (collision with different project)
2. Non-code edex projects move to ~/Documents/research/studio/
3. Research content reorganised within ~/Documents/research/
4. Teaching content reorganised within ~/Documents/teaching/
5. ~/Documents/business renamed to ~/Documents/admin
6. Cruft deleted: edx, paperless-ngx, personal/\_\_MACOSX, notes, md-scratch

## Verification checklist

- [ ] All git repos still work (spot check with `git status`)
- [ ] No broken symlinks
- [ ] Update any hardcoded paths in scripts/configs
- [ ] Set up rclone backup config for new structure

## Final structure

### ~/Code/ (after changes)

```
~/Code/
├── elixir/
│   ├── (existing)
│   ├── cozzieloops/
│   ├── fair_dinkum_game/   # renamed from fair_dinkum (collision)
│   ├── langchain/
│   ├── presserbot/
│   ├── presserbot-crawler/
│   └── swarm_grid/
├── js/
│   ├── (existing)
│   └── ascii-experiments/
└── (other language folders unchanged)
```

### ~/Documents/research/

```
~/Documents/research/
├── papers/
├── grants/
├── conferences/
├── students/
│   ├── theses/
│   └── confirmations/
├── talks/
│   ├── blurbs/
│   └── rscs-researcher-slides/
├── website/              # from benswift.me
├── service/
│   ├── cecs-website-tiger-team/
│   ├── socy-phd-convenor/
│   └── reviews/ace-26/
├── studio/
│   ├── admin/            # from studio-documents
│   ├── ai-art/
│   ├── ccc-studio/
│   ├── ccc-studio-website/
│   ├── cybernetic-futures/
│   ├── human-scale-ai/
│   ├── llms-unplugged/
│   └── (other non-code studio projects from edex)
├── tools/
├── media/
├── admin/anu-internal/
└── archive/
    ├── ben-phd-2008-2012/
    ├── extemporelang/
    ├── SuperCollider/
    ├── R/
    └── (other old projects)
```

### ~/Documents/teaching/

```
~/Documents/teaching/
├── admin/
│   ├── awards/
│   ├── curriculum-design/
│   ├── course-peer-reviews/
│   ├── higher-education-academy/
│   └── letters/
├── tools/
│   ├── comp-course-homepage/
│   ├── marking/
│   ├── enrolment-datavis/
│   ├── cs-projects-visualisation/
│   └── misc/              # other tools
└── archive/
    ├── comp1720/
    ├── comp1100-2020-s2/
    ├── comp2710-lens-2021/
    ├── comp4610-2021/
    ├── extn1019/
    ├── socy-adire/
    ├── ccc-studio/
    ├── cs-outreach-hub/
    └── comp-course-grade-bimodality/
```

### ~/Documents/ (slimmed down)

```
~/Documents/
├── admin/                # renamed from business
├── research/             # reorganised
├── teaching/             # reorganised
├── personal/
├── church/
├── org/
├── eBooks/
├── Calibre Library/
├── Music/
├── Native Instruments/
├── Paperless/
└── (other app data: ATEM, Blackmagic, Digital Editions, etc.)
```

## Acceptance criteria

- [ ] ~/Documents/research/ exists with papers, grants, conferences, studio,
      archive subdirs
- [ ] ~/Documents/teaching/ exists with admin, tools, archive subdirs
- [ ] Software projects from edex are in ~/Code/elixir/ and ~/Code/js/
- [ ] ~/Documents/ contains only app data, personal docs, research, teaching,
      and admin
- [ ] No orphaned empty directories
- [ ] Git repos verified working
