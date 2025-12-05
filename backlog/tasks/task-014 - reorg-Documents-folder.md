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
~/Code/           # Active coding projects (by language) - keep as-is
~/Documents/      # Non-code documents, app data, media
~/Research/       # Academic research (NEW)
~/Teaching/       # Course materials (NEW)
```

## Implementation plan

### Phase 1: create new directories

```bash
mkdir -p ~/Research/{papers,grants,conferences,students/confirmations,talks,website,service/reviews,studio/admin,tools,media,admin/anu-internal,archive}
mkdir -p ~/Teaching/{courses,admin,tools,archive}
```

### Phase 2: move software projects from edex to ~/Code/

```bash
mv ~/Documents/edex/cozzieloops ~/Code/elixir/
mv ~/Documents/edex/fair_dinkum ~/Code/elixir/
mv ~/Documents/edex/langchain ~/Code/elixir/
mv ~/Documents/edex/presserbot ~/Code/elixir/
mv ~/Documents/edex/presserbot-crawler ~/Code/elixir/
mv ~/Documents/edex/swarm_grid ~/Code/elixir/
mv ~/Documents/edex/ascii-experiments ~/Code/js/
```

### Phase 3: move non-software edex projects to ~/Research/studio/

```bash
mv ~/Documents/edex/ai-art ~/Research/studio/
mv ~/Documents/edex/australian-cybernetic-scripts ~/Research/studio/
mv ~/Documents/edex/cybernetic-futures ~/Research/studio/
mv ~/Documents/edex/human-scale-ai ~/Research/studio/
mv ~/Documents/edex/llms-unplugged ~/Research/studio/
mv ~/Documents/edex/metrics ~/Research/studio/
mv ~/Documents/edex/nga ~/Research/studio/
mv ~/Documents/edex/panic ~/Research/studio/
mv ~/Documents/edex/recruitment ~/Research/studio/
mv ~/Documents/edex/reg ~/Research/studio/
mv ~/Documents/edex/shitposting ~/Research/studio/
mv ~/Documents/edex/studio-bites ~/Research/studio/
mv ~/Documents/edex/systems-modelling ~/Research/studio/
mv ~/Documents/edex/working_with_name_for_dummies ~/Research/studio/
mv ~/Documents/studio-documents/* ~/Research/studio/admin/
rmdir ~/Documents/studio-documents
```

### Phase 4: move research content to ~/Research/

```bash
# Main content
mv ~/Documents/research/papers ~/Research/
mv ~/Documents/research/grants ~/Research/
mv ~/Documents/research/conferences ~/Research/
mv ~/Documents/research/student-theses ~/Research/students/theses
mv ~/Documents/research/blurbs ~/Research/talks/blurbs
mv ~/Documents/research/rscs-researcher-slides ~/Research/talks/
mv ~/Documents/research/benswift.me ~/Research/website
mv ~/Documents/research/tools ~/Research/
mv ~/Documents/research/media ~/Research/
mv ~/Documents/research/anu-internal-documents ~/Research/admin/anu-internal

# Studio projects from research
mv ~/Documents/research/ccc-studio ~/Research/studio/
mv ~/Documents/research/ccc-studio-website-eleventy ~/Research/studio/ccc-studio-website

# Service
mv ~/Documents/service/* ~/Research/service/
rmdir ~/Documents/service
mv ~/Documents/ace-26-reviews ~/Research/service/reviews/ace-26

# Archive (old/inactive)
mv ~/Documents/research/archive/* ~/Research/archive/
mv ~/Documents/research/R ~/Research/archive/
mv ~/Documents/research/SuperCollider ~/Research/archive/
mv ~/Documents/research/altair-sizing-examples ~/Research/archive/
mv ~/Documents/research/anu-cs-blog ~/Research/archive/
mv ~/Documents/research/code2k18 ~/Research/archive/
mv ~/Documents/research/codeisbeautiful ~/Research/archive/
mv ~/Documents/research/covid19-taskforce ~/Research/archive/
mv ~/Documents/research/extemporelang ~/Research/archive/
mv ~/Documents/research/francophone-pronunciation-app ~/Research/archive/francophone-app
mv ~/Documents/research/ggerganov ~/Research/archive/
mv ~/Documents/research/godot ~/Research/archive/
mv ~/Documents/research/jekyll-fontawesome-svg ~/Research/archive/
mv ~/Documents/research/microbit ~/Research/archive/
mv ~/Documents/research/ned ~/Research/archive/
mv ~/Documents/research/skeleton-pic ~/Research/archive/
mv ~/Documents/research/tidalcycles ~/Research/archive/

# Loose PDFs
mv ~/Documents/*.pdf ~/Research/students/confirmations/

# Clean up empty research folder
rmdir ~/Documents/research
```

### Phase 5: move teaching content to ~/Teaching/

```bash
# Admin
mv ~/Documents/teaching/awards ~/Teaching/admin/
mv ~/Documents/teaching/curriculum-design ~/Teaching/admin/
mv ~/Documents/teaching/course-peer-reviews ~/Teaching/admin/
mv ~/Documents/teaching/higher-education-academy ~/Teaching/admin/
mv ~/Documents/teaching/letters ~/Teaching/admin/

# Tools
mv ~/Documents/teaching/comp-course-homepage ~/Teaching/tools/
mv ~/Documents/teaching/marking ~/Teaching/tools/
mv ~/Documents/teaching/enrolment-datavis ~/Teaching/tools/
mv ~/Documents/teaching/cs-projects-visualisation ~/Teaching/tools/
mv ~/Documents/teaching/tools ~/Teaching/tools/misc

# Archive (all courses, since none are currently active)
mv ~/Documents/teaching/archive/* ~/Teaching/archive/
mv ~/Documents/teaching/comp1100-2020-s2-website ~/Teaching/archive/comp1100-2020-s2
mv ~/Documents/teaching/comp2710-lens-2021 ~/Teaching/archive/
mv ~/Documents/teaching/comp4610-2021 ~/Teaching/archive/
mv ~/Documents/teaching/extn1019 ~/Teaching/archive/
mv ~/Documents/teaching/socy-adire ~/Teaching/archive/
mv ~/Documents/teaching/ccc-studio ~/Teaching/archive/
mv ~/Documents/teaching/cs-outreach-hub ~/Teaching/archive/
mv ~/Documents/teaching/comp-course-grade-bimodality ~/Teaching/archive/

# Clean up empty teaching folder
rmdir ~/Documents/teaching
```

### Phase 6: rename and cleanup in ~/Documents/

```bash
# Rename business to admin
mv ~/Documents/business ~/Documents/admin

# Delete cruft
rm -rf ~/Documents/edx  # contains duplicate cozzieloops (not a git repo) and old my_first_lm
rm -rf ~/Documents/paperless-ngx
rm -rf ~/Documents/personal/__MACOSX
rm ~/Documents/REORGANISATION-PLAN.md  # no longer needed
rmdir ~/Documents/edex  # should be empty after phases 2-3
```

### Phase 7: notes consolidation (optional)

```bash
# Import obsidian content to nb if desired, then:
rm -rf ~/Documents/obsidian
```

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
│   ├── fair_dinkum/
│   ├── langchain/
│   ├── presserbot/
│   ├── presserbot-crawler/
│   └── swarm_grid/
├── js/
│   ├── (existing)
│   └── ascii-experiments/
└── (other language folders unchanged)
```

### ~/Research/

```
~/Research/
├── papers/
├── grants/
├── conferences/
├── students/
│   ├── theses/
│   └── confirmations/
├── talks/
├── website/
├── service/
│   ├── cecs-website-tiger-team/
│   ├── socy-phd-convenor/
│   └── reviews/ace-26/
├── studio/
│   ├── admin/
│   ├── ai-art/
│   ├── ccc-studio/
│   ├── cybernetic-futures/
│   ├── human-scale-ai/
│   └── (other non-code studio projects)
├── tools/
├── media/
├── admin/anu-internal/
└── archive/
    ├── ben-phd-2008-2012/
    ├── extemporelang/
    ├── SuperCollider/
    └── (other old projects)
```

### ~/Teaching/

```
~/Teaching/
├── courses/              (empty - no active courses)
├── admin/
│   ├── awards/
│   ├── curriculum-design/
│   └── letters/
├── tools/
│   ├── comp-course-homepage/
│   ├── marking/
│   └── misc/
└── archive/
    ├── comp1720/
    ├── comp1100-2020-s2/
    └── (other past courses)
```

### ~/Documents/ (slimmed down)

```
~/Documents/
├── admin/                (renamed from business/)
├── personal/
├── church/
├── org/
├── eBooks/
├── Calibre Library/
├── Music/
├── Native Instruments/
├── Paperless/
└── (other app data)
```

## Acceptance criteria

- [ ] ~/Research/ exists with papers, grants, conferences, studio, archive
      subdirs
- [ ] ~/Teaching/ exists with admin, tools, archive subdirs
- [ ] Software projects from edex are in ~/Code/elixir/ and ~/Code/js/
- [ ] ~/Documents/ contains only app data, personal docs, and admin
- [ ] No orphaned empty directories
- [ ] Git repos verified working
