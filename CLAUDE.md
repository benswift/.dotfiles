# Ben's dotfiles repo

The (public) config files I use on my machines.

**DO NOT EVER ADD RAW CREDENTIALS/PASSWORDS TO THIS REPO.**

Most `bin/` scripts carry a header comment explaining their own mechanism. This
file says where things live and which rules must not be broken; it does not
repeat what the scripts already document.

## Philosophy

- modern, zsh-only (no bash configs)
- cross-platform: macOS (Apple Silicon) and Linux
- mise-first for tool version management
- native package managers: Homebrew on macOS only, apt/dnf/etc on Linux (no
  Linuxbrew)
- easy to provision a new machine via `install.sh`

## Key commands

- `dotfiles doctor` --- check if everything is set up correctly
- `dotfiles update` --- pull latest and re-run symlinks
- `dotfiles edit` --- open dotfiles in editor
- `dotfiles cd` --- print the dotfiles directory (for `cd "$(dotfiles cd)"`)
- `./install.sh` --- bootstrap a new machine (also works via curl)
- `./create_symlinks.sh` --- create/update symlinks only

## Repo structure

Config files are symlinked into place (`~/.config/...`, `~/.zshrc`, etc.) by
@create_symlinks.sh. **Add every new link to @lib/symlink-manifest.sh** --- it
is the single source of truth, consumed by both `create_symlinks.sh` (to create)
and `dotfiles doctor` (to verify). Repo filenames usually drop the leading dot
(`~/.zshrc` -> `zshrc`).

Paths with no symlink targets, so absent from the manifest: @bin/ (on `PATH` via
@zshenv), @launchd/ (plists installed by hand, instructions in each header),
@lib/ (bash helpers), @tests/ (uv single-file tests, run directly:
`./tests/test_nb.py`), `backlog/` (Backlog.md tasks), @oxfmtrc.json (shared
markdown/TOML format config).

Editor and multiplexer config: @helix/, @zellij/, @ghostty/. Email: @mail/ (see
@mail/README.md). AI agents: @claude/, @codex/, @gemini/, @matilda/,
@agent-run/.

@.github/workflows/security.yml runs ruff, shellcheck, the package and portable
test suites, ty, a lock audit (@scripts/audit-locks.py) and gitleaks on every
push. A tool the tests call must be installed there too; the helix and nb suites
stay local (see the workflow).

## Python

- `bin/` tools are `uv run --script` with PEP 723 metadata and a committed
  `bin/<name>.lock`. After editing a dependency block, run
  `uv lock --script bin/<name>` and commit the lock.
- **Adding a `bin/` tool means adding it to `extend-include` in @ruff.toml**, or
  it is silently never linted (a glob can't work --- it would drag the bash and
  swift scripts through the Python parser).
- @mail/utils is a real package, installed editable by `install.sh`. It provides
  `mutt-compose-lsp`, which @helix/languages.toml configures as a language
  server, so a machine without it has a quietly broken editor.

## Version control

Plain git. Use `gh` for GitHub API operations, `glab` for GitLab. Two GitLab
instances are configured:

- `gitlab.comp.anu.edu.au` --- teaching repos (comp2300, comp1720, lucy, etc.)
- `gitlab.anu.edu.au` --- jekyll-anu websites and other ANU projects

Set `GITLAB_HOST` when not inside a repo (e.g.
`GITLAB_HOST=gitlab.comp.anu.edu.au glab repo list`); inside a clone, `glab`
reads it from `origin`.

## Tool management (mise)

@mise/config.toml (-> `~/.config/mise/config.toml`) sets global tool versions,
used when a project has no `mise.toml` of its own.

`~/.config/mise/config.local.toml` is machine-local, untracked, and auto-merged;
its `[env]` table is **the sole home for secrets** (`PUSHOVER_TOKEN`,
`PUSHOVER_USER_KEY`, `REPLICATE_API_TOKEN`, `ANU_PASSWORD`). Nothing here calls
1Password. Two exceptions, each read through a script whose header explains its
backend: @bin/vpn, and @bin/mail-secret for everything under @mail/.

### Package installation hierarchy

When a tool can be installed several ways, prefer in order:

1. **mise** --- tools this repo's scripts need, plus development runtimes
   (python, node, go, rust)
2. **platform package manager** (brew on macOS, apt/dnf on Linux) --- system
   utilities (curl, git, jq) and tools needing OS integration
3. **language package managers** (`uv tool`, `bun add -g`, `cargo install`) ---
   only when the tool's docs say so, or mise doesn't support it

Avoid `cargo install` for anything available via mise or brew --- it compiles
from source. Pick one method per tool and stick to it, so `dotfiles doctor` can
verify it.

## Editor (helix)

Upstream has tagged no release since 25.07.1, so @bin/helix-build compiles helix
from the commit in @helix/pinned-rev and installs `hx` into `~/.local/bin`.
**Don't put helix back in mise** --- `mise activate` puts its bin dir ahead of
`~/.local/bin`, so a mise-managed copy silently shadows the source build.

The script's header covers the mechanism and its usage (`--update` moves the
pin; commit @helix/pinned-rev so the other machines follow). **Never run
`hx --grammar build` by hand**: it writes grammars that shadow the build's own.
@tests/test_helix.py, run after every build, guards the setup.

## Formatting

@bin/claude-format is a `Write|Edit` hook running the same per-type formatters
Helix runs on save (dispatch table in the script; mirrors
@helix/languages.toml). Keep the two in sync.

**KDL files must stay fixed points of `kdlfmt format --kdl-version v1 -`.** The
`v1` is load-bearing (zellij parses v1 only); the kdl block in
@helix/languages.toml says why.

## Utilities (@bin/)

Each script's header has the detail. Notable ones:

- `dotfiles` --- doctor, update, edit
- `mailsync` --- sync all email accounts
- `agent-run` --- headless agent dispatcher (see below)
- `hx-ghostty` --- open files in helix in a Ghostty tab; `--register` (once per
  Mac) makes it macOS's default for text and source files
- `zj-switch` --- `Alt s` session switcher, live sessions MRU-first, annotated
  with each session's running agents
- `claude-turn-tracker` --- Pushover notifications, plus zellij pane rename to
  `⚠ <reason>` while an agent is blocked (undone on the next prompt)
- `pkb-agent`, `pkb-health`, `pkb-todo` --- the notebook's scheduled loops, a
  uv-tool package at @pkb/ (installed editable like mail/utils). Task
  definitions live in `~/.nb/home/tasks/`; one systemd timer (weddle only) fires
  everything due. The notebook's CLAUDE.md is the format reference;
  @pkb/README.md the map
- `dmarc-summary` --- report on the DMARC aggregate reports a Fastmail rule
  files into `~/Maildir/personal/DMARC`. Exits non-zero when a message failed
  DMARC, which under `p=reject` means it bounced
- `ai-tropes` --- screen a draft for AI-writing tells; taxonomy lives in the
  `ben:benswift-writer` skill's `ai-tropes.md`, cross-referenced by pattern id
- `pangram` --- the paid second opinion behind `ai-tropes`, against Pangram's
  detector API. Billed per word, so `--dry-run` costs the estimate first;
  `PANGRAM_API_KEY` goes in the untracked mise env block
- `ts-cat`, `lumis-parsers` --- tree-sitter highlighting for yazi previews and
  `nb show`, via lumis. Add a language to `PARSERS` in `lumis-parsers` if it's
  previewed often; add a `case` override in `ts-cat` if the extension doesn't
  map to a parser (as with `.mdx`)
- `pi-kiosk` --- flash an SD card booting a Raspberry Pi into a fullscreen
  Chromium kiosk, with wifi and Tailscale baked in (cloud-init, RPi OS Trixie+)

`nb` is a pinned `github:xwmx/nb` mise tool; @bin/nb is the launcher that makes
bare `nb` resolve the pin in non-interactive agent shells. @tests/test_nb.py
covers it against a temporary notebook, never `~/.nb`.

yazi's plugin dir is owned by `ya pkg`, which is why @yazi/ configs are
symlinked file-by-file rather than as a directory.

## AI coding agents

All of Claude Code, Codex CLI, Gemini CLI, Matilda Code and Grok Build read
`CLAUDE.md` as the project instructions file, so one file serves them all.
Tracked config: @claude/CLAUDE.md (global instructions, symlinked to both
`~/.claude/CLAUDE.md` and `~/.codex/AGENTS.md`), @claude/settings.json,
@codex/hooks.json, @gemini/settings.json, @matilda/system-defaults.json.

### Personal skills

Skills live **only** in the private `benswift/claude-plugin-personal` repo,
cloned by Claude Code to `~/.claude/plugins/marketplaces/ben/`. That clone is
the single source of truth: edit, commit and push from there, and **push
immediately** --- `claude plugin marketplace update` (run by every
`dotfiles update`) deletes and re-clones from GitHub, discarding local-only
commits. If that happens, the last-installed tree survives under
`~/.claude/plugins/cache/ben/ben/<sha>/`. Skills appear as `ben:<name>`.

@bin/sync-agent-config registers marketplaces, installs enabled plugins, and
symlinks each skill into `~/.agents/skills` so Codex and Matilda see them too.

### Headless agent dispatcher

@bin/agent-run runs unattended agent jobs; profiles in @agent-run/profiles.toml
select the runner and auth route, the caller supplies prompt, model and
permissions. Subscription profiles are `*-sub`, API-billed ones `*-api`
(@agent-run/profiles.toml's comments say what each clears). `openrouter` and
`deepseek` reach third-party endpoints through Claude Code's
Anthropic-compatible surface; put their keys in the untracked mise env.

A caller that passes no `--profile` (and no `AGENT_PROFILE`) gets the registry's
`default`, which is the one line to change when the subscription provider
changes. Options are namespaced per runner (`--claude-*`, `--codex-*`,
`--grok-*`) and a mismatched one is refused, not dropped. `--bypass-permissions`
is the deliberate exception --- runner-agnostic, so an unattended caller need
not know its runner.

```sh
agent-run --profile claude-sub --model sonnet \
  --claude-dangerously-skip-permissions "/find-gigs"
agent-run --profile openrouter --model "provider/model:free" "prompt"
```

### Session log analytics

@bin/ship-claude-logs sends each host's Claude and Codex session logs to weddle
hourly (systemd timer there; on macOS a step of the push-to-weddle launchd job).
@bin/ingest-claude-logs summarises them into `~/claude-logs/analytics.db` and
fails when a host goes quiet, so weddle's ingester is every other host's alarm.
Both headers have the detail.

### Per-agent notes

- **Claude Code** --- `.claude/` (with dot) is machine-local and gitignored;
  this repo tracks only `.claude/settings.json`, which disables the `impeccable`
  plugin (its `PostToolUse` hook fires on every edit and drops
  `.impeccable/hook.cache.json` into whatever it thinks the project root is,
  including `mail/utils/src/`, from where it shipped in the built wheel).
- **Codex** --- `~/.codex/config.toml` is entirely machine-local and
  deliberately untracked; set `project_doc_fallback_filenames = ["CLAUDE.md"]`
  there on each machine. There is no shared Codex profile: the `oy` alias just
  selects full-access mode.
- **Grok Build** --- installed via mise's aqua backend so it's pinned rather
  than self-updating. `grok login --device-auth` works headless. Reads this
  repo's `CLAUDE.md` and Claude Code skills with no extra config; `grok inspect`
  prints what it found.
- **Matilda Code** --- `~/.matilda/settings.json` is Matilda's own; the portable
  half is @matilda/system-defaults.json, read in place (see the `[env]` comment
  in @mise/config.toml). Keep that file in Matilda's exact byte format, stamp
  and missing trailing newline included.
  - headless login is loopback-OAuth only, no device-code fallback.
    `MATILDA_AUTH_SKIP_BROWSER=1` (`AUTH`, not `OAUTH`) still prints an
    authorize URL pointing at a random `127.0.0.1:<port>` on that host. Open it
    on a machine with a browser, then either `curl` the failed
    `.../callback?code=...` on the headless host or `ssh -L` the port first ---
    the waiting process holds the PKCE verifier.
  - there's also an OpenAI-protocol endpoint at
    `https://matilda.maincode.com/api/v1/code` (model `matilda-code-1.0`, keyed
    by `MATILDA_API_KEY`) on the same subscription.

## Email

@mail/ holds neomutt config, @mail/msmtprc (SMTP), @mail/mbsyncrc (IMAP), and
email-processing scripts in @mail/utils/. Setup instructions: @mail/README.md.

To drive neomutt (or any TUI) interactively, use the `ht-mcp` MCP server
(`mcp__ht-mcp__*` tools): create a session with `["bash"]`, run
`TERM=xterm-direct neomutt`, then send keys and take snapshots. Snapshots are
plain text and do **not** show the cursor row (it's a background colour), so
probe position with a side-effect key like `<space>` (tag-entry) and see which
row gets the `*`.

## Microsoft 365

ANU locks down third-party Graph app registrations and the device-code flow, so
**there is no API path to ANU calendar or Teams data --- don't reach for
Graph.**

There is no calendar tool, and don't write one against EventKit: ANU's Exchange
sync into macOS Calendar.app is too flaky to give answers worth trusting (it is
reliable on iOS, which is where Ben reads the work calendar).
