## About me

My name is Ben. I am an expert programmer with a PhD in Computer Science and 20
years experience in software development.

## Agent workflow principles

### Think before coding

Follow a brainstorm → plan → execute cycle:

- brainstorm: discuss the approach, identify potential issues, consider
  alternatives
- plan: break down the work into concrete steps (use TodoWrite for multi-step
  tasks)
- execute: implement, test, verify

For unfamiliar code or ambiguous requirements, add an exploration phase:

- explore: use the Explore agent to understand codebase structure, locate
  relevant files, and identify patterns
- then proceed with brainstorm → plan → execute

Do not jump straight to implementation. The time spent planning prevents costly
rewrites.

### Agent delegation

Delegate to specialized agents when their expertise matches the task:

- use language-specific agents (python-engineer, elixir-ash-phoenix-developer,
  frontend-web-engineer) for substantial work in those domains
- use github-explorer for investigating git history, comparing repos, or
  debugging GitHub Actions
- use project-manager-backlog for creating/managing backlog tasks
- use benswift-writer for any prose beyond brief technical communication
- use nb-notebook-manager for searching my notes and bookmarks

Keep tasks in the main conversation when they're simple, cross-cutting, or
require tight integration with ongoing work.

### Context management

Balance context efficiency with thoroughness:

- for targeted searches (finding a specific function/class), use Glob or Grep
  directly rather than launching an agent
- for open-ended exploration ("how does authentication work?", "what's the
  testing structure?"), use the Explore agent
- read multiple potentially relevant files in parallel when gathering context
- avoid re-reading files you've already seen unless they may have changed

### Error recovery

When encountering failures:

- read and analyze error messages completely---do not skim or assume
- if a test fails, read the full test output including backtraces before
  attempting fixes
- if a tool fails (e.g., Edit tool can't find the string), re-read the file to
  see the current state
- do not make multiple fix attempts without understanding why the previous
  attempt failed
- when blocked after 2-3 attempts, explain the situation and ask for guidance
  rather than continuing to guess

## Communication and progress

### During work

- provide brief status updates before starting time-consuming operations
- for multi-step tasks, use TodoWrite to track progress and give visibility
- do not narrate every small action, but do explain key decisions and approaches
- when you discover new information that changes the approach, explain what you
  learned and how it affects the plan

### Asking for clarification

Ask before proceeding when:

- requirements are ambiguous and multiple valid interpretations exist
- the task would modify significant amounts of code or multiple files
- you've found an issue with the existing code that wasn't part of the original
  task
- a destructive operation is needed (e.g., force push, data deletion)

Do not ask for permission for routine operations clearly implied by the task.

### Verification and confidence

Before marking work complete:

- run all relevant tests and verify pristine output
- if you modified build configuration or dependencies, run the build
- for UI changes, verify the changes work as intended (use appropriate tools)
- check that you haven't introduced unintended side effects in related code
- briefly review what was accomplished and whether it fully addresses the
  requirements

If you cannot fully verify your changes (e.g., no tests exist, can't run the
application), explicitly state what verification you were unable to perform.

## Writing rules

- use clear, concise language and don't use unnecessary adjectives (think
  Hemingway, not Joyce)
- use Australian English spelling
- don't overuse lists (either bulleted/numbered or even just enumerating things
  in prose)
- do not ever use exclamation marks
- when you need to write prose, use the benswift-writer agent

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
  - **This should be a heading as well (NOT just strong/bold text, i.e. don't do
    this)**

## Tool efficiency

### Parallel execution

When multiple independent operations are needed, execute them in parallel within
a single message:

- reading multiple files to gather context
- running git status, git diff, and git log simultaneously
- searching for multiple patterns across the codebase
- launching multiple independent agents

Do not execute dependent operations in parallel (e.g., don't run git commit
before git add completes).

### Tool selection

Choose the right tool for the task:

- Read for viewing specific files (not cat/head/tail)
- Glob for finding files by pattern (not find/ls)
- Grep for searching file contents (not grep/rg/ag commands)
- Bash for actual system commands that require shell execution (git, build
  tools, package managers)

For code modifications, prefer tools in this order:

1. ast-grep (sg) for structural code operations (finding functions, classes,
   imports; refactoring across files)
2. sed/awk for text-based edits when ast-grep isn't suitable
3. Edit tool for manual precise replacements in specific files

Never use bash echo or command-line tools to communicate with me---just output
text directly.

## Coding rules

- prefer simple, clean, maintainable solutions over clever or complex ones
- pure functions processing built-in data structures is a good practice (as Rich
  Hickey would say: "derived data, flowing")
- when modifying code, match the style and formatting of surrounding code
- never make code changes that aren't directly related to the task you're
  currently assigned
- do not write up a summary of the changes in a separate md file unless
  explicitly asked to
- no comments in code unless explicitly requested---the code should be
  self-documenting through good naming and structure

### Performance and optimisation

- prioritise correctness and maintainability over performance
- only optimise when there's a demonstrated performance problem
- if performance is part of the task requirements, measure before and after
- use appropriate data structures (don't use O(n) operations in loops when O(1)
  alternatives exist)
- but avoid premature optimisation that sacrifices clarity

### Testing

Test coverage and quality:

- tests must cover the functionality being implemented
- do not use mocks for testing unless explicitly instructed
- prefer integration tests over unit tests when both provide similar confidence
- test behaviour and outcomes, not implementation details

Test-driven development:

- when adding new functionality, consider writing the test first (RED phase)
- implement the minimal code to make it pass (GREEN phase)
- then refactor if needed while keeping tests green
- this cycle helps clarify requirements and ensures good test coverage

Test execution and output:

- never ignore test output---logs and error messages contain critical
  information
- pristine output means zero failures, zero errors, zero warnings, and zero
  backtraces in logs
- if tests produce warnings or deprecation notices, fix them
- never mark a test as "skip" unless explicitly instructed
- if a test fails intermittently, investigate and fix the flakiness rather than
  ignoring it

When tests fail:

- read the complete error output including backtraces
- identify the root cause before attempting a fix
- if the test is revealing a real bug in your implementation, fix the
  implementation
- do not modify tests to make them pass unless the test itself is incorrect

### Code search and modification

When working with code structure (finding or modifying functions, classes,
method calls, imports), always reach for ast-grep (sg) first:

- sg can find code by syntax patterns, not just text
- sg can rewrite code while preserving formatting and structure
- sg understands language semantics, avoiding false matches in strings or
  comments

Fall back to text-based tools only when necessary:

- sed/awk for edits that aren't structural (e.g., changing prose in comments,
  updating URLs)
- ensure globs are precise to avoid unintended changes

Other tools:

- gh for interacting with GitHub repositories (e.g., gh repo view user/repo
  shows the README)

### Git workflow

- write concise, imperative-mood commit messages (e.g. "fix auth bug", not
  "fixed auth bug" or "fixing auth bug")
- prefer small, focused commits over large omnibus commits
- never commit directly to main/master unless explicitly instructed
- always rebase, never merge (unless it's the _only_ way, and even then ask for
  confirmation)

### Security

- never add raw credentials, passwords, API keys, or tokens to code or config
  files
- use environment variables or secure credential stores for sensitive data
- if you encounter credentials in code during modifications, warn about them
- do not commit files that likely contain secrets (.env, credentials.json, etc.)
- when implementing authentication or authorisation, follow established patterns
  in the codebase
