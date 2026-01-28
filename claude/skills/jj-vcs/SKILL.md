---
name: jj-vcs
description:
  Uses Jujutsu (jj), a Git-compatible version control system. Covers daily
  workflow, bookmarks, revsets, conflict resolution, GitHub integration, and
  migrating existing Git repos to colocated jj. Use when working with jj or
  when a repo has a .jj directory.
---

You are an expert in Jujutsu (jj), a modern Git-compatible version control
system. You use `jj` for all version control operations when a `.jj` directory
exists in the repo. You still use `gh` CLI for GitHub API operations (PRs,
issues, etc.).

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

## Migrating a published git repo to jj

This is the recommended approach for repos already published on GitHub. It uses
colocated mode so that `.jj` and `.git` coexist, preserving full compatibility
with GitHub, CI, and collaborators using git.

### Step 1: initialise jj in the existing repo

```
cd /path/to/repo
jj git init --colocate
```

This creates a `.jj/` directory alongside the existing `.git/`. All existing git
history, branches, and tags are imported automatically.

### Step 2: verify

```
jj log                 # should show full history
jj bookmark list       # should show all git branches as bookmarks
jj st                  # should show clean working copy
```

### Step 3: configure gitignore

Add `.jj/` to the project's `.gitignore` (jj does this automatically on init,
but verify):

```
echo '.jj/' >> .gitignore
```

### Step 4: start using jj

From this point, use `jj` for all VCS operations. The colocated mode
automatically syncs between jj and git on every command, so:

- `jj git push` updates the remote git branches
- `jj git fetch` imports remote changes
- collaborators using plain git see normal git history
- CI/CD works unchanged
- GitHub PRs work via bookmarks (see above)

### What collaborators see

Nothing changes for git users. jj commits are stored as regular git commits.
The `.jj/` directory is gitignored. Bookmarks pushed by jj appear as normal git
branches on the remote.

### Limitations of colocated mode

- no submodule support (they won't appear in working copy but aren't lost)
- `.gitattributes` are not supported
- git hooks are not triggered by jj commands
- interleaving `jj` and `git` commands on the same repo can cause bookmark
  conflicts; prefer using only `jj` for VCS operations
- shared filesystem scenarios (NFS, Dropbox) can cause issues with concurrent
  access

### Rolling back

If you decide to stop using jj, simply delete the `.jj/` directory. The git
repo is fully intact and independent.

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
rather than raw content. You can switch to git-style markers with:

```toml
[ui]
conflict-marker-style = "git"
```

## Configuration

User config location: `jj config path --user`

Useful settings:

```toml
[user]
name = "Your Name"
email = "your@email.com"

[ui]
editor = "hx"                    # preferred editor
default-command = ["log"]        # run on bare `jj`
conflict-marker-style = "diff"   # default; or "snapshot", "git"
```

Per-repo config: `jj config edit --repo` (stored in `.jj/repo/config.toml`).

## Best practices

- Use `jj describe` to write meaningful change descriptions in imperative mood
- Use `jj new` to start fresh changes rather than accumulating edits
- Prefer small, focused changes that each do one thing
- Use bookmarks only when you need named references (for pushing, PRs)
- Use `jj squash` to fold work-in-progress into a parent change
- Rebase frequently with `jj rebase -d main` to stay current
- If something goes wrong, `jj op log` and `jj op undo` are your safety net
