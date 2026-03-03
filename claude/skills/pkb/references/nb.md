# nb CLI reference

nb is a command-line note-taking, bookmarking, and knowledge base tool. All data
lives as plain text files in `~/.nb`. Each notebook is a git repository. Items
are referenced by id number, filename, or title.

All operations are non-interactive --- never use flags that open an editor or
pager.

## Identifying items

Use id numbers when you already know them. To find an item, use `nb list` or
`nb search`.

## Searching

```bash
nb search "query"                    # full text search
nb search "term1" "term2"            # AND search
nb search "term1" --or "term2"       # OR search
nb search "term1" --not "excluded"   # exclusion
nb search "query" --list             # compact output (no excerpt)
nb search --tag tag1,tag2            # search by tags
nb search --type bookmark            # filter by type
nb search --all "query"              # search all notebooks
nb search "[[projects/panic]]" --list  # find wiki-link references
```

## Listing

```bash
nb list                              # list current notebook
nb list folder/                      # list folder contents
nb list --type bookmark              # filter by type
nb list --type todo                  # list todos
nb list --limit 10                   # limit results
nb list --no-color                   # plain text output
```

## Reading

```bash
nb show 42 --print                   # print note contents (no pager)
nb show 42 --path                    # get file path (for editing)
nb show 42 --title                   # print title
nb show 42 --id                      # print id
nb show 42 --print --no-color        # clean text output
```

**Critical**: when editing a file, always use `nb show <id> --path` to get the
path first, then use Read/Edit tools on that path. Never use `nb show` output
directly for editing --- it contains ANSI colour codes.

## Creating

```bash
nb add --title "My note" --content "Body."
nb add --title "Tagged" --content "Body." --tags tag1,tag2
nb add --filename "name.md" --content "Content."
nb add folder/ --title "Note in folder" --content "Body."
nb todo add "Task description."
nb bookmark "https://example.com" --tags tag1,tag2 --comment "Why."
```

## Updating

```bash
nb edit 42 --content "Appended text."
nb edit 42 --content "Replaced." --overwrite
nb edit 42 --content "Prepended." --prepend
```

For complex edits, get the file path with `nb show <id> --path` and edit
directly.

## Moving and deleting

```bash
nb move 42 folder/ --force           # move to folder
nb move 42 other-notebook: --force   # move to different notebook
nb delete 42 --force                 # delete (--force skips prompt)
```

## Daily notes

```bash
nb daily                             # open today's daily
nb daily --content "New entry."      # append to today's daily
```

Dailies live at the top level with filename pattern `Daily YYYY-MM-DD.md`.

## Todos

```bash
nb todo add "Task description."
nb todos open                        # list open todos
nb todo do 3                         # mark as done
```

## Sync

`NB_AUTO_SYNC` is disabled. A daily scheduled task runs `nb sync --all`.

## Backlog

The `nb backlog` plugin creates backlog.md-compatible task files in `backlog/`.

```bash
nb backlog "Project title"
nb backlog "Project title" --priority high --labels web,backend
nb backlog list
```
