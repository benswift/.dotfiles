## About me

My name is Ben. I am an expert programmer with a PhD in Computer Science and 20
years experience in software development.

## Agent workflow

### Asking for help

Ask before proceeding when requirements are ambiguous, the task would modify
significant code, or you've found issues beyond the original task. If blocked
after 2-3 attempts on the same problem, explain the situation and ask rather
than continuing to guess.

## Writing rules

Use clear, concise language with Australian English spelling. Don't overuse
lists or exclamation marks.

For prose work, the skill chain is: draft with benswift-writer (voice and
content quirks --- dry humour, footnotes, technical confidence), then run
jamesian (BALANCED preset by default; LAYERED for academic). Jamesian owns
sentence and paragraph architecture, em-dash discipline (sparing), opening
patterns, and signposting --- don't reintroduce those decisions inside the
benswift-writer overlay.

### Markdown formatting

Use three dashes for em dashes. Lists: don't capitalise the first letter unless
it's a full sentence ending with a period. Use sentence case for headings. Use
actual headings, not bold text as a substitute.

## Tool use

Prefer ast-grep (sg) for structural operations (functions, classes, method
calls, imports)---it understands language semantics and preserves formatting.
Fall back to sed/awk for non-structural text edits, then the Edit tool for
manual replacements.

## Language and framework preferences

- **mise**: always available via global config (`~/.config/mise/config.toml`).
  When a project has its own `mise.toml`, prefix commands with `mise exec --`
- **Python**: _always_ use `uv`, Python 3.12+ with type hints, prefer polars
  over pandas, pydantic for validation, httpx over requests, pytest with
  parallel execution
- **Elixir/Phoenix**: read the usage_rules, use Ash declarative resources,
  Phoenix LiveView, use generators, PhoenixTest syntax, tidewave for debugging
- **Frontend web**: semantic HTML with accessibility, modern browser features,
  Grid/Flexbox, modern ES6+ with functional patterns. Use the `agent-browser`
  CLI (not Playwright directly) for browser automation and UI testing
- **Tasks**: use `backlog` MCP server
- **Notes**: use `nb` CLI, get paths with `nb show <id> --path` before editing

## Coding rules

Prefer simple, clean, maintainable solutions. Pure functions processing
built-in data structures is good practice. Match the style of surrounding
code.

### Testing

Prefer integration tests over unit tests when both provide similar confidence.
Test behaviour and outcomes, not implementation details. Pristine output means
zero failures, errors, warnings, and backtraces.

### Version control

Write concise, imperative-mood commit messages. Prefer small, focused commits.
Always rebase, never merge (unless it's the only way, then ask for
confirmation).

### Security

Never put raw credentials, passwords, API keys, or tokens in code or config
files. For project secrets, use fnox with the 1Password CLI backend: reference
values as `op://Vault/Item/field` in `fnox.toml` and load through a fnox
profile, never inline. Warn if you encounter raw credentials during
modifications and don't commit files that likely contain secrets.
