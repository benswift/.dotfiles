"""briefing.md: the one notebook file the scheduled loops write.

Each loop owns a named section between HTML-comment markers and replaces it
in place, so the file never grows, never duplicates a block, and a loop that
stops running leaves a visibly dated section behind rather than nothing.
Sections render in SECTION_ORDER; anything unknown goes after them.
"""

from __future__ import annotations

import re
from datetime import datetime

from pkb_tools.notebook import notebook_dir

FILENAME = "briefing.md"
SECTION_ORDER = ("mail", "health", "retro")
HEADER = """# Briefing

Generated on weddle by the pkb loops (see [[ea-workflow]]); every section is
replaced on its own schedule, so edits here do not survive. Act on it from any
agent session with the `pkb` skill, or by hand.
"""

_SECTION = re.compile(
    r"<!-- pkb:(?P<name>[a-z-]+) -->\n(?P<body>.*?)<!-- /pkb:(?P=name) -->\n?",
    re.DOTALL,
)


def parse(text: str) -> dict[str, str]:
    return {
        m.group("name"): m.group("body").rstrip("\n") for m in _SECTION.finditer(text)
    }


def render(sections: dict[str, str]) -> str:
    ordered = [n for n in SECTION_ORDER if n in sections]
    ordered += [n for n in sections if n not in SECTION_ORDER]
    parts = [HEADER]
    for name in ordered:
        parts.append(
            f"<!-- pkb:{name} -->\n{sections[name].rstrip()}\n<!-- /pkb:{name} -->\n"
        )
    return "\n".join(parts)


def stamp(now: datetime | None = None) -> str:
    now = now or datetime.now().astimezone()
    return f"_updated {now:%a %Y-%m-%d %H:%M}_"


def read_section(name: str) -> str | None:
    path = notebook_dir() / FILENAME
    if not path.exists():
        return None
    return parse(path.read_text()).get(name)


def write_section(name: str, body: str) -> None:
    """Replace one section, creating the file if needed."""
    path = notebook_dir() / FILENAME
    sections = parse(path.read_text()) if path.exists() else {}
    sections[name] = body.rstrip() + "\n\n" + stamp()
    path.write_text(render(sections))
