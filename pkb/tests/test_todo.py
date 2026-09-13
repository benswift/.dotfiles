from __future__ import annotations

import re
from pathlib import Path

import pytest
from pkb_tools import todo


def write(root: Path, name: str, text: str) -> Path:
    path = root / name
    path.write_text(text)
    return path


def test_lists_three_sections_newest_first(notebook: Path) -> None:
    write(notebook, "20260101000000.todo.md", "# [ ] old human\n")
    write(notebook, "20260102000000.todo.md", "# [x] done\n")
    write(
        notebook,
        "20260103000000.todo.md",
        "# [ ] waiting on someone\n\n## Tags\n\n#blocked\n",
    )
    write(
        notebook,
        "20260104000000.todo.md",
        "# [ ] x.service failed\n\n## Description\n\nsee journal\n\n## Tags\n\n#bot #oncall\n",
    )
    write(notebook, "20260105000000.todo.md", "# [ ] new human\n")
    (notebook / ".index").write_text(
        "20260101000000.todo.md\n20260102000000.todo.md\n"
        "20260103000000.todo.md\n20260104000000.todo.md\n20260105000000.todo.md\n"
    )
    assert todo.render(todo.open_todos(notebook)) == (
        "\n"
        "todos\n"
        "      5  new human\n"
        "      1  old human\n"
        "\n"
        "blocked\n"
        "      3  waiting on someone\n"
        "\n"
        "bot\n"
        "      4  x.service failed\n"
    )


def test_empty_sections_are_omitted(notebook: Path) -> None:
    write(notebook, "20260101000000.todo.md", "# [ ] only one\n")
    (notebook / ".index").write_text("20260101000000.todo.md\n")
    assert todo.render(todo.open_todos(notebook)) == "\ntodos\n      1  only one\n"
    assert todo.render([]) == "(no open todos)\n"


def test_hashtag_in_a_url_is_not_a_tag() -> None:
    assert todo.TAG_RE.findall("see https://x.org/p#bot and ## Tags") == []
    assert todo.TAG_RE.findall("#bot #on-call") == ["bot", "on-call"]


def test_block_and_unblock_round_trip_a_bare_todo() -> None:
    text = "# [ ] plain\n"
    blocked = todo.add_tag(text, "blocked")
    assert blocked == "# [ ] plain\n\n## Tags\n\n#blocked\n"
    assert todo.add_tag(blocked, "blocked") == blocked
    assert todo.remove_tag(blocked, "blocked") == text


def test_block_squashes_nb_trailing_blank_lines() -> None:
    assert todo.add_tag("# [ ] from nb\n\n", "blocked") == (
        "# [ ] from nb\n\n## Tags\n\n#blocked\n"
    )


def test_a_verb_without_an_id_is_usage_not_a_new_todo(notebook: Path) -> None:
    assert todo.main(["do"]) == 2
    assert list(notebook.glob("*.todo.md")) == []


def test_block_extends_an_existing_tag_line() -> None:
    text = "# [ ] tagged\n\n## Tags\n\n#oncall\n"
    blocked = todo.add_tag(text, "blocked")
    assert blocked == "# [ ] tagged\n\n## Tags\n\n#oncall #blocked\n"
    assert todo.remove_tag(blocked, "blocked") == text


def test_block_via_main_edits_the_file_by_id(notebook: Path) -> None:
    path = write(notebook, "20260101000000.todo.md", "# [ ] plain\n")
    (notebook / ".index").write_text("20260101000000.todo.md\n")
    assert todo.main(["block", "1"]) == 0
    assert "#blocked" in path.read_text()
    assert todo.main(["unblock", "1"]) == 0
    assert path.read_text() == "# [ ] plain\n"


def bot(root: Path, stamp: str, title: str) -> Path:
    return write(root, f"{stamp}.todo.md", f"# [ ] {title}\n\n## Tags\n\n#bot\n")


def test_repeated_bot_failures_collapse_to_the_newest(notebook: Path) -> None:
    bot(notebook, "20260101000000", "x.service failed")
    bot(notebook, "20260102000000", "x.service failed")
    bot(notebook, "20260103000000", "y.service failed")
    bot(notebook, "20260104000000", "x.service failed")
    write(notebook, "20260105000000.todo.md", "# [ ] same title twice\n")
    write(notebook, "20260106000000.todo.md", "# [ ] same title twice\n")
    (notebook / ".index").write_text(
        "\n".join(f"202601{d:02}000000.todo.md" for d in range(1, 7)) + "\n"
    )
    assert todo.render(todo.open_todos(notebook)) == (
        "\n"
        "todos\n"
        "      6  same title twice\n"
        "      5  same title twice\n"
        "\n"
        "bot\n"
        "      4  x.service failed  ×3\n"
        "      3  y.service failed\n"
    )
    todos = todo.open_todos(notebook)
    newest = next(t for t in todos if t.id == 4)
    assert sorted(t.id or 0 for t in todo.duplicates(todos, newest)) == [1, 2, 4]
    human = next(t for t in todos if t.id == 6)
    assert todo.duplicates(todos, human) == [human]


def test_color_only_decorates(notebook: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    write(notebook, "20260101000000.todo.md", "# [ ] plain\n")
    (notebook / ".index").write_text("20260101000000.todo.md\n")
    todos = todo.open_todos(notebook)
    monkeypatch.setenv("COLORTERM", "truecolor")
    coloured = todo.render(todos, color=True)
    assert "\033[1;38;2;180;190;254mtodos\033[0m" in coloured
    assert re.sub(r"\033\[[0-9;]*m", "", coloured) == todo.render(todos)
    monkeypatch.delenv("COLORTERM")
    assert "\033[1;35mtodos\033[0m" in todo.render(todos, color=True)
