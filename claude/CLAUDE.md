## About Me

My name is Ben. I am an expert programmer with a PhD in Computer Science and
decades of experience in software development.

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
- to view/edit files in GitHub repositories, you have access to the `gh`
  command-line tool

## Task management

- if there's a `backlog/config.yml` file in the project, use `backlog` for task
  tracking
- create tasks as markdown files in `backlog/tasks/` using the CLI tool (a
  complete example is
  `backlog task create "Task Name" -d "Description" -s "To Do" -l auth --priority high --ac "Must work" --notes "Initial setup done" --dep task-1 -p 14`,
  although tasks can be created with as little as just a name)
- update the md file e.g. `task-12 - Fix typo.md`) with notes relating to the
  task (either WIP or once complete) and change status (e.g. from
  `"In Progress"` to `"Done"`) as appropriate

## Getting help

- ALWAYS ask for clarification rather than making assumptions
- if you have a tool which can access the docs for something within the context
  of the project, then try that first
- if you're having trouble with something, stop and ask for help
