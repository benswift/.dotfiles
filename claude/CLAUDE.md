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
content quirks --- plain, technically confident prose grounded in concrete
specifics, footnotes, extensive links), then run jamesian (BALANCED preset by
default; LAYERED for academic). Jamesian owns sentence and paragraph
architecture, em-dash discipline (sparing), opening patterns, and signposting
--- don't reintroduce those decisions inside the benswift-writer overlay.

### Markdown formatting

Use three dashes for em dashes. Lists: don't capitalise the first letter unless
it's a full sentence ending with a period. Use sentence case for headings. Use
actual headings, not bold text as a substitute.

## Tool use

Default to ast-grep (`sg`) whenever the target is a code symbol --- a
function/class/method definition, its callers, an import, a rename, or "show me
the body of X" from a large file. Treat any task that names a code construct as
a structural query, not a string search. The `ast-grep` skill carries the
pattern/rule syntax --- invoke it rather than guessing.

Reserve `rg`/`grep` for plain text searches over unstructured content,
`sed`/`awk` for non-structural text edits, and the Edit tool for manual
replacements.

## Language and framework preferences

- **mise**: always available via global config (`~/.config/mise/config.toml`).
  When a project has its own `mise.toml`, prefix commands with `mise exec --`
- **Python**: _always_ use `uv`, Python 3.12+ with type hints, fail fast (let
  exceptions bubble, no bare `except`). Prefer polars over pandas,
  pydantic/sqlmodel for validation, httpx over requests, typer for CLIs, loguru
  for logging, pathlib over os.path, pytest with parallel execution
- **Elixir/Phoenix**: read the usage_rules, use Ash declarative resources,
  Phoenix LiveView, use generators, PhoenixTest syntax, tidewave for debugging
- **Frontend web**: semantic HTML with accessibility, modern browser features,
  Grid/Flexbox, modern ES6+ with functional patterns. Use the `agent-browser`
  CLI (not Playwright directly) for browser automation and UI testing
- **Tasks**: use the `backlog` CLI (Backlog.md); the
  `ben:project-manager-backlog` skill covers task format and workflow
- **Notes**: my personal knowledge base lives at `~/.nb/home/` and is managed
  via the `nb` CLI. Use the `ben:pkb` skill whenever a task involves journals,
  dailies, todos, notes, people/, projects/, or the knowledge base. Get paths
  with `nb show <id> --path` before editing

## Coding rules

Prefer simple, clean, maintainable solutions. Pure functions processing built-in
data structures is good practice. Match the style of surrounding code.

### Testing

Prefer integration tests over unit tests when both provide similar confidence.
Test behaviour and outcomes, not implementation details. Pristine output means
zero failures, errors, warnings, and backtraces.

### Version control

Write concise, imperative-mood commit messages with a `scope: description`
subject: the scope names the part of the codebase that changed (subsystem,
module, or file), which is what a reader scanning the log actually cares about.
Give every commit a scope --- one without it is like a sentence without a
subject. Don't use Conventional Commits type prefixes (`feat:`, `fix:`,
`chore:`); the type wastes the subject line and a clear description makes the
change obvious anyway. Prefer small, focused commits. Stage specific files by
name rather than `git add .` or `-A`. Always rebase, never merge (unless it's
the only way, then ask for confirmation); force-push only with
`--force-with-lease`, never a bare `--force`.

During long-running agentic work, commit as you go rather than waiting to be
asked: land a commit at each logical checkpoint, but only when the full suite of
tests, lints, and other checks passes --- never commit a red state. Commit to
the current branch (don't branch first) and commit locally only; leave pushing
to me as a deliberate step.

### Formatter hook

A PostToolUse `Write|Edit` hook (`~/.dotfiles/bin/claude-format`) reformats
every file I edit, running the same per-type formatter Helix runs on save so the
two never fight over formatting (`oxfmt-helix` for markdown/toml, `ruff format`
for Python, `typstyle --wrap-text` for Typst, and so on). The catch: the first
edit to a file that isn't already a fixed point of its formatter reflows the
whole file, so a one-line change can land as a 100--300 line diff mixing the
reflow with the real edit. Don't bundle the two. Reproduce the pure reflow from
HEAD with the file's EXACT dispatch-table command (never a guessed flag --- a
mismatched invocation formats differently and fights the hook), commit that
reflow alone, then commit the semantic change on top and check the second diff
shows only your edit. If matching the hook is impractical, commit the working
(hook-formatted) file and note the bundled reflow --- it still lands as a fresh
fixed point, so later edits diff clean.

### Security

Never put raw credentials, passwords, API keys, or tokens in code or config
files. For project secrets, use fnox with the 1Password CLI backend on macOS:
reference values as `op://Vault/Item/field` in `fnox.toml` and load through a
fnox profile, never inline. On Linux it's ok to have things in an untracked mise
env block. Warn if you encounter raw credentials during modifications and don't
commit files that likely contain secrets.
