---
name: vcs
description:
  Version control using Jujutsu (jj) or Git depending on the repo. Covers daily
  workflow, branching, rebasing, conflict resolution, GitHub integration via gh
  CLI, and migrating Git repos to colocated jj. Use when working with version
  control, jj, git, or GitHub.
---

You are an expert in version control. You choose the right tool based on the
repo:

- If a `.jj` directory exists, use `jj` for all version control operations
- Otherwise, use `git`
- Always use `gh` CLI for GitHub API operations (PRs, issues, etc.)

Never mix `jj` and raw `git` commands in a colocated jj repo --- it can cause
bookmark synchronisation issues.

---

# Jujutsu (jj)

## Mental model

jj differs from git in several fundamental ways. Internalise these before
running any commands.

### The working copy is a commit

Every change to the working copy is automatically snapshotted on the next `jj`
command. There is no staging area --- `jj diff` shows the difference between the
parent commit and the current working-copy commit. You never need to "add" files;
new files are tracked automatically (controlled by `snapshot.auto-track` and
`.gitignore`).

### Changes, not commits

jj works with *changes* identified by a stable change ID, not commit hashes. A
change's commit hash changes every time it's rewritten (rebase, amend, squash),
but its change ID stays the same. Use change IDs (`@`, `@-`, short change ID
prefixes) to refer to revisions.

### First-class conflicts

Conflicts are stored in the commit, not as working-copy markers that block
further operations. Rebasing a conflict doesn't produce nested markers.
Operations never fail due to conflicts --- descendants are automatically
rebased. Resolve conflicts by editing the conflicted commit directly.

### Operation log

Every `jj` command creates an operation in the log (`jj op log`). Any operation
can be undone with `jj op undo` or restored with `jj op restore <op-id>`. This
makes experimentation safe.

## Core commands

### Daily workflow

```
jj st                              # status
jj diff                            # working copy changes
jj log                             # history (default revset)
jj log -r <revset>                 # history for specific revisions
jj describe -m "message"           # set description on working copy
jj new                             # start a new change on top of current
jj new <rev>                       # start a new change on top of <rev>
jj edit <rev>                      # check out an existing change for editing
jj squash                          # squash working copy into parent
jj squash --into <rev>             # squash working copy into a specific change
jj split                           # interactively split working copy
jj abandon <rev>                   # discard a change
jj undo                            # undo the last operation
```

### Bookmarks (analogous to git branches)

Bookmarks are named pointers to revisions. They map to git branches when
pushing/fetching.

```
jj bookmark list                   # list all bookmarks
jj bookmark create <name> -r <rev> # create bookmark pointing at revision
jj bookmark set <name> -r <rev>    # move bookmark to revision
jj bookmark delete <name>          # delete bookmark
jj bookmark track <name>@<remote>  # track a remote bookmark locally
```

Bookmarks automatically follow when their target change is rewritten. When you
push bookmark `foo`, it updates the `foo` branch on the remote.

### Rebasing

```
jj rebase -d <destination>         # rebase current change onto destination
jj rebase -s <source> -d <dest>    # rebase source and descendants
jj rebase -r <rev> -d <dest>       # rebase single change (re-parents children)
```

Rebasing always succeeds, even with conflicts. Conflicts become first-class
data in the commit and descendants automatically rebase.

### Git interop

```
jj git fetch                       # fetch from remote (no implicit rebase)
jj git push                        # push tracked bookmarks
jj git push -c <rev>               # auto-create bookmark and push
jj git push --bookmark <name>      # push specific bookmark
```

There is no `jj git pull`. Instead: `jj git fetch` then
`jj rebase -d <bookmark>` (e.g. `jj rebase -d main`).

## Revset language

Revsets select sets of commits. Common patterns:

| Expression | Meaning |
|---|---|
| `@` | working copy |
| `@-` | parent of working copy |
| `@--` | grandparent |
| `root()..@` | all ancestors of working copy |
| `@..` | all descendants of working copy |
| `bookmarks()` | all local bookmarks |
| `remote_bookmarks()` | all remote bookmarks |
| `main..@` | commits between main and working copy |
| `heads(all())` | all head commits |
| `conflicts()` | commits with unresolved conflicts |
| `description(pattern)` | commits matching description |
| `author(pattern)` | commits by author |
| `files(path)` | commits touching path |

Operators: `x & y` (intersection), `x | y` (union), `~x` (complement),
`x-` (parents), `x+` (children).

## Working with GitHub

### Pushing changes for a PR

Option 1 --- named bookmark:

```
jj new main
# ... make changes ...
jj describe -m "add feature X"
jj bookmark create my-feature -r @
jj git push --bookmark my-feature
# create PR with: gh pr create
```

Option 2 --- auto-generated bookmark:

```
jj new main
# ... make changes ...
jj describe -m "add feature X"
jj git push -c @      # creates and pushes a bookmark automatically
```

### Updating a PR after review

Edit the change directly (rewriting history):

```
jj edit <change-id>    # check out the change
# ... make edits ...
jj describe -m "updated message"  # optional
jj git push --bookmark <bookmark-name>
```

Or add a new commit on top:

```
jj new <change-id>
# ... make changes ...
jj bookmark set <bookmark-name> -r @
jj git push --bookmark <bookmark-name>
```

### Syncing with upstream

```
jj git fetch
jj rebase -d main      # rebase working copy onto updated main
```

### Stacked PRs

Each change in the stack gets its own bookmark. Push them all:

```
jj new main
jj describe -m "first change"
jj bookmark create stack-1 -r @
jj new
jj describe -m "second change"
jj bookmark create stack-2 -r @
jj git push --bookmark stack-1 --bookmark stack-2
```

After upstream merges the first PR, rebase:

```
jj git fetch
jj rebase -d main
```

## Migrating a git repo to jj

See [references/migration.md](references/migration.md) for the full colocated
migration guide.

## Conflict resolution

When a rebase or merge produces conflicts:

```
jj log -r 'conflicts()'   # find conflicted changes
jj edit <conflicted-rev>   # check out the conflicted change
# edit files to resolve (conflict markers use <<<<<<< / %%%%%%% / >>>>>>> format)
# jj automatically snapshots your resolution
jj st                      # verify no remaining conflicts
```

jj's conflict markers differ from git's. The `%%%%%%%` sections show diffs
rather than raw content.

## jj best practices

- Use `jj describe` to write meaningful change descriptions in imperative mood
- Use `jj new` to start fresh changes rather than accumulating edits
- Prefer small, focused changes that each do one thing
- Use bookmarks only when you need named references (for pushing, PRs)
- Use `jj squash` to fold work-in-progress into a parent change
- Rebase frequently with `jj rebase -d main` to stay current
- If something goes wrong, `jj op log` and `jj op undo` are your safety net
- **Avoid interactive flags** (`--interactive`, `-i`) --- they require a TTY and
  will fail in non-interactive environments. Instead of `jj split --interactive`,
  use `jj new @-` then `jj restore --from <change>` to manually move files
  between changes

---

# Git

Use git when a repo has no `.jj` directory. The goal is a clean, linear history.

## Rules

- **Always rebase, never merge.** Only merge when genuinely unavoidable (e.g.
  integrating a long-lived divergent branch where rebase would lose meaningful
  context), and ask for confirmation first.
- **Always `git pull --rebase`**, never plain `git pull`.
- **Stage specific files** by name, not `git add .` or `git add -A`.
- **Use `--force-with-lease`** when force-pushing after a rebase, never bare
  `--force`.
- **Write concise commit messages** in imperative mood ("add feature", not
  "added feature").
- **Make small, focused commits** that each do one thing.
- **Don't commit** generated files, build artefacts, or secrets.
- **Clean up feature branches** after merging.
- **Avoid interactive flags** (`-i`) --- they require a TTY and will fail in
  non-interactive environments.

## Syncing with upstream

```
git fetch origin
git rebase origin/main             # rebase current branch onto main
git pull --rebase                  # shorthand: fetch + rebase
git push --force-with-lease        # force push safely after rebase
```

## Undoing things

```
git commit --amend                 # amend the last commit
git reset HEAD~1                   # undo last commit, keep changes staged
git reset --mixed HEAD~1           # undo last commit, unstage changes
git reflog                         # find lost commits
```

---

# GitHub (gh CLI)

Use `gh` for all GitHub API interactions regardless of whether the repo uses jj
or git. Prefer `--rebase` when merging PRs via `gh pr merge`.
