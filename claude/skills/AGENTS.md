# Skills directory

This directory contains skills that Claude Code and Codex CLI load dynamically
based on task context. Skills are model-invoked---Claude decides when to use
them based on the description field.

## Directory structure

Each skill lives in its own subdirectory with a `SKILL.md` file:

```
skills/
├── skill-name/
│   ├── SKILL.md           # required: main instructions
│   ├── REFERENCE.md       # optional: supplementary docs
│   └── scripts/           # optional: utility scripts
└── another-skill/
    └── SKILL.md
```

## SKILL.md format

Every skill requires a `SKILL.md` file with YAML frontmatter:

```markdown
---
name: skill-name
description:
  What this skill does and when to use it. Be specific about triggers.
---

Instructions for Claude when this skill is active...
```

### Frontmatter fields

- **name** (required): lowercase letters, numbers, and hyphens only (max 64
  chars). Use gerund form where appropriate (e.g. `processing-pdfs`).
- **description** (required): max 1024 chars. Must include both what the skill
  does and when to use it. Write in third person ("Processes PDF files..." not
  "I can process PDFs...").
- **allowed-tools** (optional, Claude Code only): restrict which tools the skill
  can use without permission.

### Description guidelines

The description is critical for skill discovery---Claude uses it to decide which
skill to activate from potentially many available skills.

Good example:

```yaml
description:
  Extracts text and tables from PDF files, fills forms, and merges documents.
  Use when working with PDF files or when the user mentions PDFs, forms, or
  document extraction.
```

Bad example:

```yaml
description: Helps with documents
```

## Writing effective skills

### Keep it concise

Claude is already capable. Only add context Claude doesn't have:

- Domain-specific knowledge
- Project conventions
- Preferred tools and patterns
- Workflows that require specific sequences

### Match specificity to fragility

- **High freedom** (text instructions): when multiple approaches are valid
- **Medium freedom** (templates/pseudocode): when patterns exist but variation
  is acceptable
- **Low freedom** (exact scripts): when operations are fragile or sequence
  matters

### Progressive disclosure

Keep `SKILL.md` under 500 lines. For larger skills:

- Put the overview and quick-start in `SKILL.md`
- Reference additional files: "See [REFERENCE.md](REFERENCE.md) for API details"
- Claude loads referenced files only when needed

### Avoid

- Time-sensitive information ("before August 2025, use...")
- Inconsistent terminology
- Deeply nested references (keep one level deep from SKILL.md)
- Windows-style paths (always use forward slashes)
- Offering too many options without a clear default

## Testing skills

Skills behave differently across models. Test with the models you plan to use:

- Haiku: may need more explicit guidance
- Sonnet: balanced
- Opus: avoid over-explaining

## Compatibility

These skills work with both Claude Code and Codex CLI (with `--enable skills`).
The format is portable across Claude apps, Claude Code, and the API.
