## About Me

My name is Ben. I am an expert programmer with a PhD in Computer Science and
decades of experience in software development.

## General coding principles

- Prefer simple, clean, maintainable solutions over clever or complex ones, even
  if the latter are more concise or performant. Readability and maintainability
  are primary concerns.
- Make the smallest reasonable changes to get to the desired outcome. You MUST
  ask permission before reimplementing features or systems from scratch instead
  of updating the existing implementation.
- When modifying code, match the style and formatting of surrounding code, even
  if it differs from standard style guides. Consistency within a file is more
  important than strict adherence to external standards.
- NEVER make code changes that aren't directly related to the task you're
  currently assigned. If you notice something that should be fixed but is
  unrelated to your current task, document it in a new issue instead of fixing
  it immediately.
- NEVER remove code comments unless you can prove that they are actively false.
  Comments are important documentation and should be preserved even if they seem
  redundant or unnecessary to you.
- When writing comments, avoid referring to temporal context about refactors or
  recent changes. Comments should be evergreen and describe the code as it is,
  not how it evolved or was recently changed.
- NEVER implement a mock mode for testing or for any purpose. We always use real
  data and real APIs, never mock implementations.
- NEVER name things as 'improved' or 'new' or 'enhanced', etc. Code naming
  should be evergreen. What is new today will be "old" someday.
- DO NOT write up a summary of the changes in a separate md file unless
  explicitly asked to.

## Development Workflow: Spec → Code

THESE INSTRUCTIONS ARE CRITICAL!

They dramatically improve the quality of the work you create.

### Phase 1: Requirements First

When asked to implement any feature or make changes, ALWAYS start by asking:
"Should I create a Spec for this task first?"

IFF user agrees:

- create a markdown file in `.cursor/scopes/feature-name.md`
- interview the user to clarify:
  - purpose & user problem
  - success criteria
  - scope & constraints
  - technical considerations
  - out of scope items

### Phase 2: Review & Refine

After drafting the Spec:

- present it to the user
- ask: "Does this capture your intent? Any changes needed?"
- iterate until user approves
- end with: "Spec looks good? Type 'GO!' when ready to implement"

### Phase 3: Implementation

ONLY after user types "GO!" or explicitly approves:

- begin coding based on the Spec
- reference the Spec for decisions
- update Spec if scope changes, but ask user first

### File Organization

```
.cursor/
├── scopes/
│ ├── feature-name.md # shared/committed Specs
│ └── .local/ # Git-ignored experimental Specs
│ └── Experiment.md
```

**Remember: Think first, ask clarifying questions, _then_ code. The Spec is your
north star.**

### Anchor comments

Add specially formatted comments throughout the codebase, where appropriate, for
yourself as inline knowledge that can be easily `grep`ped for.

- use `AIDEV-NOTE:`, `AIDEV-TODO:`, or `AIDEV-QUESTION:` comments aimed at AI
  and developers
- keep them concise (≤ 120 chars)
- **important:** before scanning files, always first try to **locate existing
  anchors** `AIDEV-*` in relevant subdirectories
- **update relevant anchors** when modifying associated code
- **do not remove `AIDEV-NOTE`s** without explicit human instruction

## Testing

- tests MUST cover the functionality being implemented
- NEVER ignore the output of the system or the tests - Logs and messages often
  contain CRITICAL information.
- TEST OUTPUT MUST BE PRISTINE TO PASS (no failures or error backtraces in logs)

## Tool use

- use the tools available to you in a standard unix shell environment whenever
  appropriate (e.g. moving files with `mv` rather than echoing the code
  directly)
- use sed/awk for surgical edits in the codebase (although ensure that any globs
  are as narrow as possible to avoid changing unintended files)

## Getting help

- ALWAYS ask for clarification rather than making assumptions.
- if you have a tool which can access the docs for something within the context
  of the project, then try that first
- if you're having trouble with something, stop and ask for help
