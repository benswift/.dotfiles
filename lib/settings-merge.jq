# Merge a repo's settings layers into a tool's live settings file (the input).
# Ownership is per top-level key:
#   $enforced[0]  the repo's keys: each replaces the live value wholesale, and a
#                 null deletes the key
#   $seed[0]      written only where the live file lacks the key, so a tool
#                 command (`/model`) can change it for good
# Every other live key is the machine's and is kept. `$schema` in either layer
# is for editors and never written. Used by create_symlinks.sh, and by
# `dotfiles doctor` (a merge that changes nothing means the machine is in sync).

def layer: (. // {}) | del(.["$schema"]);

($enforced[0] | layer) as $e
| ($seed[0] | layer) + . + $e
| delpaths([$e | to_entries[] | select(.value == null) | [.key]])
