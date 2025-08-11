## About Me

My name is Ben. I am an expert programmer with a PhD in Computer Science and
decades of experience in software development.

## Writing rules

- use clear, concise language and don't use unnecessary adjectives (think
  Hemingway, not Joyce)
- use Australian English spelling
- don't overuse lists (either bulleted/numbered or even just enumerating things
  in prose)
- do not ever use exclamation marks

### Markdown formatting

- use three dashes for an em dash, e.g. "he said---and this is the main
  point---that..."
- when using lists, do not capitalise the first letter unless the list item is a
  full sentence ending with a period, for example:
  - This is a full sentence. (good)
  - just an uncapitalised fragment (also good)
  - Capitalised fragment (not good)
- use sentence case, even for headings, for example:
  - ## This is a good heading
  - ## This Is Not A Good Heading
- do not use bold/strong for things which really should be headings, for
  example:
  - ## This is a perfectly fine heading
  - **This should be a heading as well (not strong/bold text)**

## General coding principles

- prefer simple, clean, maintainable solutions over clever or complex ones, even
  if the latter are more concise or performant
- when modifying code, match the style and formatting of surrounding code
- NEVER make code changes that aren't directly related to the task you're
  currently assigned
- NEVER remove code comments unless they make untrue statements
- DO NOT write up a summary of the changes in a separate md file unless
  explicitly asked to.

## Testing

- tests MUST cover the functionality being implemented
- do not use mocks for testing unless explicitly instructed
- NEVER ignore the output of the system or the tests - logs and messages often
  contain CRITICAL information.
- TEST OUTPUT MUST BE PRISTINE (ZERO failures or error backtraces in logs)

## Tools and shell commands

- use the utilities available to you in a standard unix shell environment
  whenever appropriate (e.g. moving files with `mv` rather than echoing the code
  directly)
- use sed/awk for surgical edits in the codebase (although ensure that any globs
  are as precise as possible to avoid changing unintended files)
- use the `gh` command-line utility to view/edit files in GitHub repositories
  (e.g. `gh repo view user/repo` will show the project's README in md format)

## Task management

- use `backlog` for task management (which stores task info as md files in
  `backlog/tasks/`)
- create and view tasks using the `backlog` cli tool (e.g.
  `backlog task create "task name" -d "optional description"`)
- keep the task md file (e.g. `backlog/tasks/task-7 - fix typo.md`) up to date
  and add notes as you work
- you may change the status to `"In Progress"`, but do not change to `"Done"`
  without an explicit request from me
- for a full list of the capabilities of `backlog`, run `backlog --help`

### Create spec tasks

THESE INSTRUCTIONS ARE CRITICAL! They dramatically improve the quality of the
work you create.

If asked to "create a spec for..." followed by the description of a feature or
change:

1. create a new task (with an appropriate name) with `backlog task create ...`

2. interview the user to clarify:

   - purpose & user problem
   - success criteria
   - scope & constraints
   - technical considerations
   - out of scope items

3. after drafting the spec in the task's md file:

   - present it to the user
   - ask: "Does this capture your intent? Any changes needed?"
   - iterate until user approves
   - end with: "Spec looks good? Type 'GO!' when ready to implement"

4. ONLY after user types "GO!" or explicitly approves:

   - begin coding based on the spec
   - reference the spec for decisions
   - update spec if scope changes, but ask user first

**Remember: Think first, ask clarifying questions, _then_ code. The spec is your
north star.**
