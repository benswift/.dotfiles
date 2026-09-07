"""pkb-briefing: read or replace a section of briefing.md from the shell.

  pkb-briefing show [<section>]   print the file, or one section's body
  pkb-briefing set <section>      replace the section with stdin

The `set` form is how a prompt-driven task publishes to the briefing without
touching the markers itself.
"""

from __future__ import annotations

import sys

from pkb_tools import briefing
from pkb_tools.notebook import notebook_dir


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    match args:
        case ["show"]:
            path = notebook_dir() / briefing.FILENAME
            print(path.read_text() if path.exists() else "(no briefing yet)")
            return 0
        case ["show", name]:
            body = briefing.read_section(name)
            if body is None:
                print(f"pkb-briefing: no section {name!r}", file=sys.stderr)
                return 1
            print(body)
            return 0
        case ["set", name]:
            briefing.write_section(name, sys.stdin.read())
            return 0
        case _:
            print(__doc__, file=sys.stderr)
            return 2


if __name__ == "__main__":
    sys.exit(main())
