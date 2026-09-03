#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pexpect", "pyte", "pytest", "pytest-xdist"]
# ///
"""Integration-test the source-built helix against this repo's editor config.

bin/helix-build runs this after every build. The point is not to test helix ---
upstream does that --- but to catch the ways a new main commit can quietly
break *this* setup: a config key renamed out from under helix/config.toml, a
custom grammar that stops compiling, a runtime directory that stops being
searched, or a glob whose semantics shifted so that `*.deck.mdx` no longer
resolves to astromotion-deck.

File-type detection has no headless interface, so those cases drive a real hx
in a pty and read the language back off the statusline.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

import pexpect
import pyte
import pytest
import tomllib

ROOT = Path(__file__).parent.parent
LANGUAGES_TOML = ROOT / "helix" / "languages.toml"
CONFIG_TOML = ROOT / "helix" / "config.toml"
PIN_FILE = ROOT / "helix" / "pinned-rev"
QUERY_DIR = ROOT / "helix" / "runtime" / "queries"

# Kept in step with bin/helix-build, which owns both paths.
HELIX_SRC = Path(
    os.environ.get("HELIX_SRC", Path.home() / ".local" / "share" / "helix-src")
)
HX = Path(os.environ.get("HELIX_PREFIX", Path.home() / ".local")) / "bin" / "hx"

# Grammars built here outrank the ones the build produced (helix searches the
# config dir ahead of the baked runtime), so the directory must stay absent.
SHADOWING_GRAMMARS = ROOT / "helix" / "runtime" / "grammars"

DYLIB = "dylib" if sys.platform == "darwin" else "so"

ANSI = re.compile(r"\x1b\[[0-9;?]*[a-zA-Z]|\x1b\][^\x07]*\x07|\x1b[()][AB0]")

# Terminal queries and mode switches, which pyte either mis-dispatches or
# does not need: private CSI (cursor visibility, alt screen, synchronised
# output, device-status probes), DCS and OSC strings. Stripping them leaves
# the plain cursor-movement and SGR stream that paints the grid.
CHATTER = re.compile(
    r"\x1b\[\?[0-9;]*[a-zA-Z$]|\x1bP.*?(?:\x1b\\|\x07)|\x1b\][^\x07\x1b]*(?:\x07|\x1b\\)",
    re.DOTALL,
)

# One file per fiddly rule in helix/languages.toml, and the language it must
# resolve to. These globs are the part of the config most likely to break
# silently, because nothing else reports on them.
DETECTION_CASES = [
    ("slides.deck.mdx", "astromotion-deck"),
    ("decks/nested/partial.mdx", "astromotion-deck"),
    ("plain.mdx", "mdx"),
    ("proj/backlog/task-1.md", "backlog-task"),
    ("README.md", "markdown"),
    ("PULLREQ_EDITMSG", "markdown"),
    ("show.deck.svx", "markdown"),
    ("neomutt-abc123", "mutt-compose"),
    ("muttrc", "muttrc"),
    ("thing.xtm", "extempore"),
    ("mod.py", "python"),
]

# Valid input for each language that declares a formatter, so the round-trip
# exercises the real parser rather than an empty-input fast path.
FORMATTER_SAMPLES = {
    "markdown": "# Heading\n\nsome  prose\n",
    "mdx": "# Heading\n\n{/* _class: hero */}\n\nsome  prose\n",
    "astromotion-deck": "# Slide\n\n{/* _class: hero */}\n\nsome  prose\n",
    "toml": 'a  =  1\n[table]\nb = "two"\n',
    "kdl": 'node "value" {\n  child 1\n}\n',
    "typst": "= Heading\n\nSome prose.\n",
    "latex": "\\documentclass{article}\n\\begin{document}\nHi\n\\end{document}\n",
    "astro": "---\nconst x = 1;\n---\n\n<h1>{x}</h1>\n",
    "typescript": "const x   =  1;\n",
    "tsx": "const x = () => <div />;\n",
    "javascript": "const x   =  1;\n",
    "jsx": "const x = () => <div />;\n",
}

# Extension to hand the formatter for the %{buffer_name} expansion, where the
# language name isn't the extension.
FORMATTER_EXTENSIONS = {
    "astromotion-deck": ".deck.mdx",
    "javascript": ".js",
    "jsx": ".jsx",
    "markdown": ".md",
    "typescript": ".ts",
}


def language_config() -> dict:
    with LANGUAGES_TOML.open("rb") as handle:
        return tomllib.load(handle)


LANGUAGES = language_config().get("language", [])
LANGUAGE_NAMES = [entry["name"] for entry in LANGUAGES]
GRAMMAR_NAMES = [entry["name"] for entry in language_config().get("grammar", [])]
SERVER_COMMANDS = sorted(
    {
        server["command"]
        for server in language_config().get("language-server", {}).values()
    }
)
FORMATTED_LANGUAGES = [
    entry["name"] for entry in LANGUAGES if isinstance(entry.get("formatter"), dict)
]


def pinned_rev() -> str:
    for line in PIN_FILE.read_text().splitlines():
        if re.fullmatch(r"[0-9a-f]{40}", line.strip()):
            return line.strip()
    raise AssertionError(f"{PIN_FILE} holds no 40-character commit sha")


def health(*args: str) -> str:
    """`hx --health` output with the colour escapes stripped."""
    result = subprocess.run(
        [str(HX), "--health", *args],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, f"hx --health {' '.join(args)}:\n{result.stderr}"
    return ANSI.sub("", result.stdout + result.stderr)


def health_line(report: str, prefix: str) -> str:
    for line in report.splitlines():
        if line.startswith(prefix):
            return line
    raise AssertionError(f"no {prefix!r} line in:\n{report}")


@pytest.fixture(scope="session")
def minimal_config() -> Path:
    """A config that shows nothing but the file type, so the statusline can be
    read as the detected language and nothing else. Language servers are off:
    detection does not need them, and starting them costs seconds per case."""
    config = Path(tempfile.mkdtemp()) / "config.toml"
    config.write_text(
        "[editor.lsp]\nenable = false\n\n"
        '[editor.statusline]\nleft = []\ncenter = []\nright = ["file-type"]\n'
    )
    return config


ROWS, COLUMNS = 24, 200


def drain(child: pexpect.spawn, settle: float = 0.4, limit: float = 15.0) -> str:
    """Read until hx has been quiet for `settle` seconds.

    There is no event that says "the file is open and painted", so quiet is
    the only signal available.
    """
    output = ""
    deadline = time.monotonic() + limit
    while time.monotonic() < deadline:
        try:
            output += child.read_nonblocking(size=65536, timeout=settle)
        except pexpect.TIMEOUT:
            if output:
                break
        except pexpect.EOF:
            break
    return output


def run_helix(args: list[str]) -> list[str]:
    """Open helix with `args` and return the rendered screen, row by row.

    The output is replayed through a terminal emulator rather than pattern
    matched, because helix paints with cursor movement: the raw stream is not
    in screen order.
    """
    child = pexpect.spawn(
        str(HX),
        args,
        env=dict(os.environ, TERM="xterm-256color"),
        dimensions=(ROWS, COLUMNS),
        encoding="utf-8",
        timeout=30,
    )
    try:
        painted = drain(child)
        child.send(":q\r")
        try:
            child.expect(pexpect.EOF, timeout=5)
        except (pexpect.TIMEOUT, pexpect.EOF):
            # A config error puts helix behind a "press ENTER" prompt, which
            # swallows the :q. The force close below still reaps it.
            pass
    finally:
        child.close(force=True)

    assert painted, f"hx painted nothing for {args}"
    screen = pyte.Screen(COLUMNS, ROWS)
    pyte.Stream(screen).feed(CHATTER.sub("", painted))
    return screen.display


def detected_language(config: Path, path: Path) -> str:
    """The language helix resolved `path` to.

    With left and center empty, the statusline row holds the file type and
    nothing else --- which matters, because the language is often a substring
    of the file name.
    """
    # Second from the bottom: the command line sits below the statusline.
    return run_helix(["-c", str(config), str(path)])[-2].strip()


def test_hx_on_path_is_the_source_build() -> None:
    resolved = shutil.which("hx")
    assert resolved is not None, "hx is not on PATH --- run bin/helix-build"
    assert Path(resolved).resolve() == HX.resolve(), (
        f"PATH resolves hx to {resolved}, not the source build at {HX}."
        " A mise-managed helix shadows it: run 'mise uninstall helix'."
    )


def test_hx_matches_the_pinned_rev() -> None:
    version = subprocess.run(
        [str(HX), "--version"], capture_output=True, text=True, check=True
    ).stdout
    match = re.search(r"\(([0-9a-f]{8})\)", version)
    assert match, f"{version.strip()!r} carries no git hash --- not a source build"
    assert pinned_rev().startswith(match.group(1)), (
        f"installed helix is {match.group(1)}, pin is {pinned_rev()[:8]}"
    )


def test_runtime_directories_are_searched_in_the_right_order() -> None:
    line = health_line(health(), "Runtime directories:")
    dirs = [Path(part) for part in line.split(":", 1)[1].strip().split(";")]
    config_runtime = Path.home() / ".config" / "helix" / "runtime"
    assert dirs[0] == config_runtime, (
        f"{config_runtime} must come first, or this repo's query overrides lose"
        f" to upstream's. Order is {dirs}."
    )
    assert HELIX_SRC / "runtime" in dirs, (
        f"{HELIX_SRC / 'runtime'} is not searched --- HELIX_DEFAULT_RUNTIME was"
        f" not baked into this build. Order is {dirs}."
    )


def test_no_shadowing_grammar_directory() -> None:
    assert not SHADOWING_GRAMMARS.exists(), (
        f"{SHADOWING_GRAMMARS} outranks the built runtime and shadows every"
        " grammar with a stale copy. Delete it; never run 'hx --grammar build'"
        " by hand."
    )


def test_configured_theme_exists() -> None:
    with CONFIG_TOML.open("rb") as handle:
        theme = tomllib.load(handle).get("theme")
    assert theme, "helix/config.toml names no theme"
    assert (HELIX_SRC / "runtime" / "themes" / f"{theme}.toml").is_file(), (
        f"theme {theme!r} is gone from this helix build"
    )


def test_real_config_loads_cleanly(tmp_path: Path) -> None:
    """helix/config.toml must still be valid for this build.

    A key renamed upstream does not fail the build or `--health`: helix falls
    back to its defaults behind a "Bad config ... press ENTER" screen, which
    only shows up when you next open a file.
    """
    sample = tmp_path / "sample.md"
    sample.write_text("# Title\n\nSome prose.\n")
    rows = run_helix([str(sample)])
    complaints = [row.strip() for row in rows if "Bad config" in row]
    assert not complaints, f"helix rejected the config: {complaints}"
    assert any("# Title" in row for row in rows), (
        "helix did not open the file:\n" + "\n".join(rows)
    )


@pytest.mark.parametrize("name", GRAMMAR_NAMES)
def test_custom_grammar_is_built(name: str) -> None:
    grammar = HELIX_SRC / "runtime" / "grammars" / f"{name}.{DYLIB}"
    assert grammar.is_file(), f"{grammar} missing --- rebuild with helix-build --force"


@pytest.mark.parametrize("name", LANGUAGE_NAMES)
def test_language_parses_and_highlights(name: str) -> None:
    report = health(name)
    assert "Tree-sitter parser: ✓" in report, f"no parser for {name}:\n{report}"
    assert "Highlight queries: ✓" in report, f"no highlights for {name}:\n{report}"


@pytest.mark.parametrize("name", FORMATTED_LANGUAGES)
def test_language_formatter_resolves(name: str) -> None:
    report = health(name)
    formatter = report.split("Configured formatter:", 1)[1].split("Tree-sitter", 1)[0]
    assert "✓" in formatter, f"formatter for {name} is unresolved:\n{report}"


@pytest.mark.parametrize("command", SERVER_COMMANDS)
def test_language_server_is_installed(command: str) -> None:
    assert shutil.which(command), (
        f"helix/languages.toml configures {command!r} as a language server, but"
        " nothing on this machine provides it"
    )


def test_query_overrides_belong_to_configured_languages() -> None:
    """Every directory in helix/runtime/queries must name a language this repo
    configures --- helix ignores queries for a language it has never heard of,
    and does so silently."""
    orphans = [
        entry.name
        for entry in sorted(QUERY_DIR.iterdir())
        if entry.is_dir() and entry.name not in LANGUAGE_NAMES
    ]
    assert not orphans, f"query directories with no language block: {orphans}"


@pytest.mark.parametrize(("relative", "expected"), DETECTION_CASES)
def test_file_type_detection(
    minimal_config: Path, tmp_path: Path, relative: str, expected: str
) -> None:
    path = tmp_path / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("sample\n")
    assert detected_language(minimal_config, path) == expected


@pytest.mark.parametrize("name", FORMATTED_LANGUAGES)
def test_formatter_round_trips(tmp_path: Path, name: str) -> None:
    """Run the formatter exactly as helix/languages.toml declares it, and
    require the result to be a fixed point. A formatter that keeps changing its
    own output makes every save a whole-file diff."""
    entry = next(item for item in LANGUAGES if item["name"] == name)
    sample = FORMATTER_SAMPLES[name]
    suffix = FORMATTER_EXTENSIONS.get(name, f".{name}")
    buffer = tmp_path / f"sample{suffix}"
    buffer.write_text(sample)

    command = [entry["formatter"]["command"]]
    command += [
        str(buffer) if arg == "%{buffer_name}" else arg
        for arg in entry["formatter"].get("args", [])
    ]

    def run(text: str) -> str:
        result = subprocess.run(
            command, input=text, capture_output=True, text=True, check=False
        )
        assert result.returncode == 0, (
            f"{' '.join(command)} exited {result.returncode}:\n{result.stderr}"
        )
        return result.stdout

    once = run(sample)
    assert once.strip(), f"{' '.join(command)} produced nothing"
    assert run(once) == once, f"{' '.join(command)} is not idempotent"


def test_formatters_match_the_claude_format_hook() -> None:
    """bin/claude-format runs the same formatters on Claude's edits that helix
    runs on save; a formatter added to only one of them makes the two fight."""
    hook = (ROOT / "bin" / "claude-format").read_text()
    missing = sorted(
        {
            entry["formatter"]["command"]
            for entry in LANGUAGES
            if isinstance(entry.get("formatter"), dict)
        }
        - {word for word in re.findall(r"[\w-]+", hook)}
    )
    assert not missing, (
        f"formatters helix runs but bin/claude-format does not: {missing}"
    )


if __name__ == "__main__":
    # Extra arguments pass through, so a single case can be re-run in
    # isolation: ./tests/test_helix.py -k detection
    sys.exit(pytest.main([__file__, *(sys.argv[1:] or ["-v", "-n", "auto"])]))
