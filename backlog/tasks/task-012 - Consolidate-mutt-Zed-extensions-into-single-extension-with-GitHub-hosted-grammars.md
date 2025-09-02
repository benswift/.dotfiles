---
id: task-012
title: >-
  Consolidate mutt Zed extensions into single extension with GitHub-hosted
  grammars
status: To Do
assignee: []
created_date: '2025-09-02 23:30'
updated_date: '2025-09-02 23:30'
labels:
  - zed
  - tree-sitter
  - mutt
dependencies: []
priority: high
---

## Description

Consolidate the separate muttrc and mutt-compose Zed extensions into a single 'mutt' extension with both grammars properly hosted on GitHub repositories for better maintainability and portability

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] Single consolidated mutt extension replaces both muttrc and mutt-compose extensions
- [ ] Both tree-sitter grammars are hosted on GitHub under benswift organisation  
- [ ] Grammar repositories are properly versioned and tagged
- [ ] Extension supports all existing file patterns from both original extensions
- [ ] All syntax highlighting functionality is preserved from both original extensions
- [ ] Local file:// grammar references are eliminated
- [ ] Extension follows Zed's standard practices for multi-grammar extensions
<!-- AC:END -->

## Implementation Plan

### Phase 1: Grammar Repository Setup
1. **Create tree-sitter-muttrc repository**
   - Fork neomutt/tree-sitter-muttrc to benswift/tree-sitter-muttrc
   - Review and test current grammar functionality
   - Address any outstanding grammar issues
   - Tag initial release (v0.1.0)

2. **Create tree-sitter-mutt-compose repository**
   - Extract grammar from local `/tmp/tree-sitter-mutt-compose` location
   - Create new repository benswift/tree-sitter-mutt-compose 
   - Set up proper tree-sitter project structure (grammar.js, package.json, etc.)
   - Test grammar compilation and functionality
   - Tag initial release (v0.1.0)

### Phase 2: Consolidated Extension Development
3. **Create new consolidated extension**
   - Create `zed/extensions/mutt/` directory structure
   - Design extension.toml with both grammars referenced
   - Configure dual language support in languages/ subdirectories

4. **Migrate language configurations**
   - Copy and adapt muttrc language config (patterns: muttrc, neomuttrc, .muttrc, .neomuttrc)
   - Copy and adapt mutt-compose language config (patterns: neomutt/cache/**, tmp/neomutt-*, etc.)
   - Ensure no conflicts between language configurations
   - Preserve all existing file pattern matching

### Phase 3: Integration and Testing
5. **Test consolidated extension**
   - Verify muttrc files are properly highlighted and detected
   - Verify mutt compose buffers are properly highlighted and detected  
   - Test file pattern matching for all supported file types
   - Compare highlighting output with original extensions

6. **Clean up and documentation**
   - Remove old muttrc and mutt-compose extension directories
   - Update any references or documentation
   - Test extension installation and functionality

## Technical Details

### Current Extension Analysis
- **muttrc extension**: Uses external GitHub grammar (neomutt/tree-sitter-muttrc)
- **mutt-compose extension**: Uses local file:// grammar (non-portable)
- Both extensions are functionally independent but related

### Proposed Extension Structure
```
zed/extensions/mutt/
├── extension.toml              # Contains both grammar references
├── languages/
│   ├── muttrc/
│   │   ├── config.toml        # Muttrc language config
│   │   └── highlights.scm     # Syntax highlighting rules  
│   └── mutt-compose/
│       ├── config.toml        # Mutt compose language config
│       └── highlights.scm     # Syntax highlighting rules
```

### Grammar Hosting Strategy
- **benswift/tree-sitter-muttrc**: Fork of neomutt version for maintainability
- **benswift/tree-sitter-mutt-compose**: New repository for compose buffer grammar
- Both repos will use proper semantic versioning and GitHub releases

### File Pattern Coverage
The consolidated extension must support all existing patterns:
- **Muttrc files**: `**/muttrc`, `**/neomuttrc`, `**/.muttrc`, `**/.neomuttrc`  
- **Compose files**: `**/neomutt/cache/**/*`, `**/tmp/neomutt-*`, `**/tmp/mutt-*`
