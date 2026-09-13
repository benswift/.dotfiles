"""pkb-todo (the shell's `todo`): open todos in three sections, and the few
edits that need no editor.

  todo                  list open todos: todos, blocked, bot
  todo do <id>          mark done (nb todo do)
  todo undo <id>        reopen
  todo block <id>       tag #blocked; `unblock` removes it
  todo add              a new todo in the editor
  todo <words...>       a new todo titled from the words, no quoting needed

A todo is "bot" when it carries the #bot tag, which is how unit-oncall files
job failures; everything else is human. "blocked" is the #blocked tag on a
human todo. Listing reads the files directly: `nb todos open` takes seconds
over a notebook this size and cannot exclude a tag.
"""

from __future__ import annotations

import re
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path

from pkb_tools.notebook import notebook_dir

BOT = "bot"
BLOCKED = "blocked"
OPEN_PREFIX = "# [ ] "
TAG_RE = re.compile(r"(?<![\w#])#([A-Za-z][\w-]*)")
INDEX = ".index"


@dataclass(frozen=True)
class Todo:
    id: int | None  # nb's id: the file's 1-based line in .index
    path: Path
    title: str
    tags: frozenset[str]

    @property
    def section(self) -> str:
        if BOT in self.tags:
            return "bot"
        if BLOCKED in self.tags:
            return "blocked"
        return "todos"


def read_index(root: Path) -> dict[str, int]:
    path = root / INDEX
    if not path.exists():
        return {}
    lines = path.read_text().split("\n")
    return {name: n for n, name in enumerate(lines, start=1) if name}


def open_todos(root: Path) -> list[Todo]:
    """Open todos, newest first (nb names todo files by creation timestamp)."""
    index = read_index(root)
    todos = []
    for path in sorted(root.glob("*.todo.md"), reverse=True):
        text = path.read_text()
        first, _, _ = text.partition("\n")
        if not first.startswith(OPEN_PREFIX):
            continue
        todos.append(
            Todo(
                id=index.get(path.name),
                path=path,
                title=first.removeprefix(OPEN_PREFIX).strip(),
                tags=frozenset(TAG_RE.findall(text)),
            )
        )
    return todos


def render(todos: list[Todo]) -> str:
    if not todos:
        return "(no open todos)\n"
    out = []
    for name in ("todos", "blocked", "bot"):
        rows = [t for t in todos if t.section == name]
        if not rows:
            continue
        out.append(name)
        for t in rows:
            out.append(f"  {t.id if t.id is not None else '?':>5}  {t.title}")
        out.append("")
    return "\n".join(out)


def resolve(root: Path, id_str: str) -> Path:
    """The todo file behind an nb id, or SystemExit with a reason."""
    names = {n: name for name, n in read_index(root).items()}
    name = names.get(int(id_str))
    if name is None or not name.endswith(".todo.md"):
        sys.exit(f"todo: no todo with id {id_str}")
    return root / name


def add_tag(text: str, tag: str) -> str:
    if tag in TAG_RE.findall(text):
        return text
    text = text.rstrip("\n") + "\n"
    lines = text.split("\n")
    if "## Tags" in lines:
        # nb's own layout: the tag line is the first non-blank line after the
        # heading, so extend it rather than opening a second section.
        for i in range(lines.index("## Tags") + 1, len(lines)):
            if lines[i].strip():
                lines[i] = f"{lines[i]} #{tag}"
                return "\n".join(lines)
    return f"{text}\n## Tags\n\n#{tag}\n"


def remove_tag(text: str, tag: str) -> str:
    text = re.sub(rf"[ \t]*(?<![\w#])#{re.escape(tag)}(?![\w-])", "", text)
    # A Tags section left with nothing in it is noise, not state.
    return re.sub(r"\n+## Tags\n\s*\Z", "\n", text)


def nb_interactive(*args: str) -> int:
    """Run nb with the terminal attached, for the editor and nb's own output."""
    return subprocess.run(["nb", *args], cwd=notebook_dir(), check=False).returncode


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    root = notebook_dir()
    match args:
        case []:
            print(render(open_todos(root)), end="")
            return 0
        case ["do" | "undo" as verb, id_str] if id_str.isdigit():
            resolve(root, id_str)
            return nb_interactive("todo", verb, id_str)
        case ["block" | "unblock" as verb, id_str] if id_str.isdigit():
            path = resolve(root, id_str)
            edit = add_tag if verb == "block" else remove_tag
            path.write_text(edit(path.read_text(), BLOCKED))
            return 0
        case ["add"]:
            filename = time.strftime("%Y%m%d%H%M%S") + ".todo.md"
            return nb_interactive(
                "add", "--filename", filename, "--content", OPEN_PREFIX, "--edit"
            )
        case ["do" | "undo" | "block" | "unblock"]:
            print(__doc__, file=sys.stderr)
            return 2
        case ["add", *words] | [*words]:
            return nb_interactive("todo", "add", " ".join(words))
    return 2


if __name__ == "__main__":
    sys.exit(main())
