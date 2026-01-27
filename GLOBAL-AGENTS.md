## About me

My name is Ben. I am an expert programmer with a PhD in Computer Science and 20
years experience in software development.

## Agent workflow principles

### Think before coding

Follow a brainstorm → plan → execute cycle. For unfamiliar code or ambiguous
requirements, explore first---do not jump straight to implementation.

### Agent delegation

Delegate to specialized agents when their expertise matches the task. Use
language-specific agents for substantial work, github-explorer for git history,
project-manager-backlog for task management, benswift-writer for prose, and
nb-notebook-manager for notes. Keep tasks in the main conversation when they're
simple, cross-cutting, or require tight integration with ongoing work.

### Error recovery

Read and analyze error messages completely. If blocked after 2-3 attempts,
explain the situation and ask for guidance rather than continuing to guess.

## Communication and progress

### During work

Provide brief status updates before time-consuming operations. For multi-step
tasks, use TodoWrite to track progress. Explain key decisions and approaches,
especially when new information changes the plan.

### Asking for clarification

Ask before proceeding when requirements are ambiguous, the task would modify
significant code, you've found issues beyond the original task, or a destructive
operation is needed. Do not ask for permission for routine operations clearly
implied by the task.

### Verification and confidence

Before marking work complete, run all relevant tests and verify pristine output.
If you modified build configuration or dependencies, run the build. Check for
unintended side effects. If you cannot fully verify your changes, explicitly
state what verification you were unable to perform.

## Writing rules

Use clear, concise language with Australian English spelling. Don't overuse
lists or exclamation marks. When you need to write prose, use the
benswift-writer agent.

### Markdown formatting

Use three dashes for em dashes. Lists: don't capitalise the first letter unless
it's a full sentence ending with a period. Use sentence case for headings. Use
actual headings, not bold text as a substitute.

## Tool efficiency

### Parallel execution

When multiple independent operations are needed, execute them in parallel within
a single message. Do not execute dependent operations in parallel.

### Tool selection

Choose the right tool: Read for files, Glob for patterns, Grep for content
search, Bash for system commands. For code modifications: prefer ast-grep (sg)
for structural operations, then sed/awk for text edits, then Edit tool for
manual replacements. Never use bash echo to communicate with me---output text
directly.

## Language and framework preferences

- **mise** if a project has a `mise.toml` file then use `mise` to manage
  dependencies and environments (prefixing commands with `mise exec --`)
- **Python**: _always_ use `uv`, Python 3.12+ with type hints, prefer polars
  over pandas, pydantic for validation, httpx over requests, pytest with
  parallel execution
- **Elixir/Phoenix**: read the usage_rules, use Ash declarative resources,
  Phoenix LiveView, use generators, PhoenixTest syntax, tidewave for debugging
- **Frontend web**: semantic HTML with accessibility, modern browser features,
  Grid/Flexbox, modern ES6+ with functional patterns, Playwright for testing
- **Writing**: conversational academic voice (Australian English, em dashes,
  self-aware, direct openings)
- **Version control**: prefer `jj` when a `.jj` directory exists in the repo,
  otherwise use `git`. Use `gh` CLI for GitHub API operations (PRs, issues,
  history investigation)
- **Tasks**: use `backlog` MCP server
- **Notes**: use `nb` CLI, get paths with `nb show <id> --path` before editing

## Coding rules

Prefer simple, clean, maintainable solutions. Pure functions processing built-in
data structures is good practice. Match the style of surrounding code. Never
make unrelated changes or write summary files unless asked. No comments in code
unless requested---code should be self-documenting.

### Performance and optimisation

Prioritise correctness and maintainability over performance. Only optimise when
there's a demonstrated problem. Use appropriate data structures, but avoid
premature optimisation.

### Testing

Tests must cover the functionality being implemented. Prefer integration tests
over unit tests when both provide similar confidence. Test behaviour and
outcomes, not implementation details. Consider writing tests first (TDD).
Pristine output means zero failures, errors, warnings, and backtraces. When
tests fail, read the complete error output and identify the root cause before
attempting a fix. Never ignore test output or mark tests as skip unless
instructed.

### Code search and modification

When working with code structure (functions, classes, method calls, imports),
reach for ast-grep (sg) first. It understands language semantics and preserves
formatting. Fall back to sed/awk only for non-structural edits.

### Version control workflow

Write concise, imperative-mood commit/change descriptions. Prefer small, focused
changes. Always rebase, never merge (unless it's the only way, then ask for
confirmation). In jj repos, use `jj describe` for change descriptions and
`jj new` to start new changes.

### Security

Never add raw credentials, passwords, API keys, or tokens to code or config
files. Use environment variables or secure credential stores. Warn if you
encounter credentials during modifications. Do not commit files that likely
contain secrets.
