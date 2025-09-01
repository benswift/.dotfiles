---
id: task-012
title: Convert neomutt color theme to true color (24-bit RGB)
status: Done
assignee: []
created_date: "2025-08-31 11:29"
updated_date: "2025-08-31 11:31"
labels:
  - email
  - neomutt
  - colors
dependencies: []
---

## Description

Create a modern, visually appealing true color (24-bit RGB) theme for neomutt.
This is an opportunity to move beyond the current wombat-inspired theme and
explore popular color schemes like Monokai, Dracula, Nord, Solarized, Gruvbox,
One Dark, or create something entirely original. The goal is to leverage the
full richness that true color allows for a more beautiful and readable email
experience.

## Acceptance Criteria

<!-- AC:BEGIN -->

- [ ] Configuration includes `color_directcolor = yes` setting
- [ ] All colorable elements use #RRGGBB format instead of color0-255 palette
- [ ] Color scheme is modern and visually appealing (may be inspired by popular
      themes like Monokai, Dracula, Nord, Solarized, Gruvbox, One Dark, or be
      entirely original)
- [ ] Colors provide smooth gradients and precise color choices that showcase
      true color capabilities
- [ ] All neomutt colorable elements are themed consistently
- [ ] Configuration works correctly with truecolor-enabled neomutt build
- [ ] Color choices are documented with inspiration/rationale in configuration
      comments
- [ ] Theme improves readability and visual hierarchy compared to current setup
<!-- AC:END -->

## Implementation Plan

1. Verify neomutt is compiled with +truecolor support
2. Research popular color themes for inspiration:
   - Analyse Monokai, Dracula, Nord, Solarized, Gruvbox, One Dark colour
     palettes
   - Consider which aesthetic would work best for email reading
   - Look at existing terminal/editor implementations of these themes
3. Create comprehensive list of ALL colorable elements in neomutt including:
   - Basic elements: normal, indicator, status, tree, signature, message,
     attachment, error, tilde
   - Search and navigation: search, markers
   - Quote levels: quoted, quoted1-9
   - Index flags: ~F (flagged), ~N (new), ~O (old), ~T (tagged), ~D (deleted)
   - Header elements: hdrdefault, header patterns for date/to/from/subject/etc
   - Sidebar elements (if used)
   - Body patterns and regex matches
4. Choose or design colour scheme:
   - Select an inspiring base theme or create original palette
   - Ensure colours work well for email context (readability, hierarchy)
   - Plan colour relationships and contrast ratios
5. Add color_directcolor = yes to neomutt configuration
6. Implement the chosen colour scheme using #RRGGBB format:
   - Create cohesive colour mapping for all elements
   - Use true colour capabilities for smooth gradients and subtle variations
   - Ensure proper contrast for accessibility
7. Test configuration with actual neomutt usage across different email types
8. Document colour choices, inspiration, and rationale in configuration comments
