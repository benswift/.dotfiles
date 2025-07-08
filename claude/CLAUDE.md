## About Me

My name is Ben. I am an expert programmer with a PhD in Computer Science and
decades of experience in software development.

## Development Workflow: Spec → Code

THESE INSTRUCTIONS ARE CRITICAL!

They dramatically improve the quality of the work you create.

### Phase 1: Requirements First

When asked to implement any feature or make changes, ALWAYS start by asking:
"Should I create a Spec for this task first?"

IFF user agrees:

- create a markdown file in `spec/feature-name.md`
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

**Remember: Think first, ask clarifying questions, _then_ code. The Spec is your
north star.**

## General coding principles

- Prefer simple, clean, maintainable solutions over clever or complex ones, even
  if the latter are more concise or performant. Readability and maintainability
  are primary concerns.
- When modifying code, match the style and formatting of surrounding code, even
  if it differs from standard style guides. Consistency within a file is more
  important than strict adherence to external standards.
- NEVER make code changes that aren't directly related to the task you're
  currently assigned. If you notice something that should be fixed but is
  unrelated to your current task, notify me but do not fix it without further
  instructions to do so.
- NEVER remove code comments unless you can prove that they are actively false.
  Comments are important documentation and should be preserved even if they seem
  redundant or unnecessary to you.
- When writing comments, avoid referring to temporal context about refactors or
  recent changes. Comments should be evergreen and describe the code as it is,
  not how it evolved or was recently changed.
- NEVER implement mocks for testing or for any purpose. We always use real data
  and real APIs, never mock implementations.
- NEVER name things as 'improved' or 'new' or 'enhanced', etc. Code naming
  should be evergreen. What is new today will be "old" someday.
- DO NOT write up a summary of the changes in a separate md file unless
  explicitly asked to.

## Testing

- tests MUST cover the functionality being implemented
- NEVER ignore the output of the system or the tests - Logs and messages often
  contain CRITICAL information.
- TEST OUTPUT MUST BE PRISTINE TO PASS (ZERO failures or error backtraces in
  logs)

## Tools and shell commands

- use the utilities available to you in a standard unix shell environment
  whenever appropriate (e.g. moving files with `mv` rather than echoing the code
  directly)
- use sed/awk for surgical edits in the codebase (although ensure that any globs
  are as narrow as possible to avoid changing unintended files)
- use the `gh` command-line utility to view/edit files in GitHub repositories
  (e.g. `gh repo view user/repo` will show the project's README in md format)

## Task management

- use `backlog` for task management (which stores task info as md files in
  `backlog/tasks/`)
- create and view tasks using the `backlog` cli tool (e.g.
  `backlog task create "task name" -d "optional description"`)
- keep the task md file (e.g. `backlog/tasks/task-7 - fix typo.md`) up to date
  and add notes as you work, changing status (e.g. from `"In Progress"` to
  `"Done"`) as appropriate
