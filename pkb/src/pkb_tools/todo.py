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

Bot todos with the same title (a job that keeps failing past the 24h re-arm)
collapse to one row under the most recent id, whose journal tail is the
current one, and `do` on any of them closes the whole run.
"""

from __future__ import annotations

import os
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


def duplicates(todos: list[Todo], todo: Todo) -> list[Todo]:
    """The open bot todos sharing this bot todo's title (itself included)."""
    if BOT not in todo.tags:
        return [todo]
    return [t for t in todos if BOT in t.tags and t.title == todo.title]


def collapse(rows: list[Todo]) -> list[tuple[Todo, int]]:
    """Bot rows grouped by title, newest kept; a human row is its own group."""
    out: list[tuple[Todo, int]] = []
    seen: set[str] = set()
    for t in rows:
        if BOT not in t.tags:
            out.append((t, 1))
        elif t.title not in seen:
            seen.add(t.title)
            out.append((t, sum(1 for u in rows if u.title == t.title)))
    return out


# Catppuccin Mocha, the theme every terminal thing in the dotfiles uses
# (ghostty, helix, zellij, neomutt, claude-statusline). Truecolour when the
# terminal says so, else the nearest of the 16 ANSI colours, which the same
# theme maps back onto the palette.
MOCHA = {
    "lavender": ((180, 190, 254), 35),
    "yellow": ((249, 226, 175), 33),
    "mauve": ((203, 166, 247), 35),
    "overlay1": ((127, 132, 156), 90),
}
SECTION_STYLE = {"todos": "lavender", "blocked": "yellow", "bot": "mauve"}


def use_color() -> bool:
    return sys.stdout.isatty() and not os.environ.get("NO_COLOR")


def sgr(name: str, bold: bool = False) -> str:
    rgb, ansi = MOCHA[name]
    truecolor = os.environ.get("COLORTERM") in ("truecolor", "24bit")
    fg = "38;2;{};{};{}".format(*rgb) if truecolor else str(ansi)
    return f"\033[{'1;' if bold else ''}{fg}m"


def render(todos: list[Todo], color: bool = False) -> str:
    def paint(name: str, text: str, bold: bool = False) -> str:
        return f"{sgr(name, bold)}{text}\033[0m" if color else text

    if not todos:
        return "(no open todos)\n"
    out = []
    for name in ("todos", "blocked", "bot"):
        rows = [t for t in todos if t.section == name]
        if not rows:
            continue
        out.append("")
        out.append(paint(SECTION_STYLE[name], name, bold=True))
        for t, n in collapse(rows):
            id_ = paint("overlay1", f"{t.id if t.id is not None else '?':>5}")
            count = paint("overlay1", f"  ×{n}") if n > 1 else ""
            out.append(f"  {id_}  {t.title}{count}")
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
            print(render(open_todos(root), color=use_color()), end="")
            return 0
        case ["do", id_str] if id_str.isdigit():
            path = resolve(root, id_str)
            todos = open_todos(root)
            target = next((t for t in todos if t.path == path), None)
            if target is None:
                return nb_interactive("todo", "do", id_str)
            for t in duplicates(todos, target):
                if nb_interactive("todo", "do", str(t.id)) != 0:
                    return 1
            return 0
        case ["undo", id_str] if id_str.isdigit():
            resolve(root, id_str)
            return nb_interactive("todo", "undo", id_str)
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
