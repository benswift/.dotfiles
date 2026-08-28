#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist"]
# ///
"""Exercise bin/ai-tropes: the structural finders, masking, and the CLI."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).parent.parent / "bin" / "ai-tropes"

_loader = importlib.machinery.SourceFileLoader("ai_tropes", str(SCRIPT))
_spec = importlib.util.spec_from_loader("ai_tropes", _loader, origin=str(SCRIPT))
assert _spec is not None and _spec.loader is not None
mod = importlib.util.module_from_spec(_spec)
sys.modules["ai_tropes"] = mod
_spec.loader.exec_module(mod)


def ids(text: str, noisy: bool = False) -> list[str]:
    """Pattern ids hit by `text`, in order, after masking."""
    enabled = mod.select("", "", noisy)
    masked = mod.mask_code(text)
    return [pattern.id for pattern, _ in mod.collect(masked, enabled)]


def notes(text: str) -> list[str]:
    enabled = mod.select("", "", True)
    masked = mod.mask_code(text)
    return [hit.note for _, hit in mod.collect(masked, enabled)]


class TestStructuralFinders:
    """The four detectors that exist because no regex can express them."""

    def test_echo_run_catches_repeated_skeleton(self):
        text = (
            "The parser is a tiny state machine. The renderer is a tiny state machine."
        )
        assert "echo-run" in ids(text)
        assert "a tiny state machine" in notes(text)[0]

    def test_echo_run_ignores_sentences_across_a_blank_line(self):
        text = (
            "The parser is a tiny state machine.\n\n"
            "The renderer is a tiny state machine."
        )
        assert "echo-run" not in ids(text)

    def test_echo_run_ignores_unrelated_adjacent_sentences(self):
        text = "The parser reads one byte at a time. Rendering happens later on."
        assert "echo-run" not in ids(text)

    def test_stacked_questions(self):
        text = "Do I know how it works? Where it breaks? Which corners it cut?"
        assert "stacked-questions" in ids(text)
        assert "3 questions in a row" in notes(text)

    def test_single_question_is_not_a_stack(self):
        assert "stacked-questions" not in ids("Do I know how it works? I do.")

    def test_anaphora_run(self):
        text = (
            "Maybe nobody needed it. Maybe the shortcut confused people. "
            "Maybe the redesign was overdue."
        )
        assert "anaphora" in ids(text)
        assert "3 sentences opening 'maybe'" in notes(text)

    def test_anaphora_needs_three(self):
        text = "Maybe nobody needed it. Maybe the shortcut confused people."
        assert "anaphora" not in ids(text)

    def test_anaphora_skips_pronouns_and_articles(self):
        text = "The cat sat. The dog left. The room emptied."
        assert "anaphora" not in ids(text)

    def test_chain_counts_items(self):
        text = "No sign-ups, no downloads, no hassle."
        assert "no-chain" in ids(text)
        assert "3 'no' items" in notes(text)

    def test_chain_needs_two_real_items(self):
        """A lone 'no' followed by an unrelated 'no longer' is not a litany."""
        text = "The repo carries no config, and doctor no longer checks for it."
        assert "1 'no' item" not in " ".join(notes(text))


class TestMasking:
    """Code is not prose: a shell snippet must not be screened as writing."""

    def test_fenced_code_is_ignored(self):
        text = "Ordinary prose here.\n\n```sh\nrg 'delve' --robust --seamless\n```\n"
        assert ids(text) == []

    def test_inline_code_is_ignored(self):
        assert ids("Pass `--leverage` to the command.") == []

    def test_link_targets_are_ignored(self):
        text = "See [the writeup](https://example.com/?utm_source=rss) for more."
        assert "chatbot-leftovers" not in ids(text)

    def test_link_text_is_still_screened(self):
        text = "See [why we delve into it](https://example.com/x) for more."
        assert "vocab-classic" in ids(text)

    def test_frontmatter_is_ignored(self):
        text = '---\ntitle: "Delve into robust systems"\n---\n\nPlain opening line.\n'
        assert ids(text) == []

    def test_masking_preserves_line_numbers(self):
        text = "```sh\necho hi\n```\n\nIt is important to note that this matters.\n"
        masked = mod.mask_code(text)
        assert len(masked) == len(text)
        enabled = mod.select("", "", False)
        _, hit = mod.collect(masked, enabled)[0]
        assert mod.line_col(masked, hit.start)[0] == 5


class TestSelection:
    def test_noisy_patterns_are_off_by_default(self):
        chosen = {p.id for p in mod.select("", "", False)}
        assert "colon-triple" not in chosen
        assert "no-chain" in chosen

    def test_noisy_patterns_can_be_enabled(self):
        assert "colon-triple" in {p.id for p in mod.select("", "", True)}

    def test_only_selects_exactly_those(self):
        assert [p.id for p in mod.select("no-chain", "", False)] == ["no-chain"]

    def test_skip_removes(self):
        assert "no-chain" not in {p.id for p in mod.select("", "no-chain", False)}

    @pytest.mark.parametrize("only,skip", [("bogus", ""), ("", "bogus")])
    def test_unknown_id_is_rejected(self, only: str, skip: str):
        with pytest.raises(ValueError, match="unknown pattern id"):
            mod.select(only, skip, False)


class TestOverlapSuppression:
    def test_one_span_is_reported_once(self):
        """'It's important to note' matches two patterns; report the first."""
        text = "It is important to note that the rollout happened in stages."
        assert ids(text).count("note-that") == 1


class TestDensity:
    def test_em_dashes_counted_in_both_spellings(self):
        d = mod.density("One --- two, and three — four, plus word---word here.")
        assert d.em_dashes == 3

    def test_repeated_word_surfaced(self):
        d = mod.density(("crumhorn " * 4) + "and some other words entirely")
        assert ("crumhorn", 4) in d.echo_words

    def test_common_words_are_not_surfaced(self):
        d = mod.density("because " * 6)
        assert d.echo_words == []


class TestCli:
    def run(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [str(SCRIPT), *args], capture_output=True, text=True, check=False
        )

    def test_clean_prose_exits_zero(self, tmp_path: Path):
        draft = tmp_path / "clean.md"
        draft.write_text(
            "This closing paragraph is deliberately ordinary, with nothing in "
            "it that any of the patterns should recognise.\n"
        )
        result = self.run(str(draft))
        assert result.returncode == 0
        assert "no hits" in result.stdout

    def test_hits_exit_nonzero(self, tmp_path: Path):
        draft = tmp_path / "sloppy.md"
        draft.write_text("No fluff, no filler, no jargon.\n")
        result = self.run(str(draft))
        assert result.returncode == 1
        assert "[no-chain]" in result.stdout

    def test_exit_zero_flag(self, tmp_path: Path):
        draft = tmp_path / "sloppy.md"
        draft.write_text("No fluff, no filler, no jargon.\n")
        assert self.run(str(draft), "--exit-zero").returncode == 0

    def test_json_output(self, tmp_path: Path):
        import json

        draft = tmp_path / "sloppy.md"
        draft.write_text("It is important to note that this shipped.\n")
        result = self.run(str(draft), "--json", "--exit-zero")
        payload = json.loads(result.stdout)
        assert payload[0]["hits"][0]["pattern"] == "note-that"
        assert payload[0]["hits"][0]["line"] == 1

    def test_list_patterns(self):
        result = self.run("--list")
        assert result.returncode == 0
        assert "echo-run" in result.stdout
        assert "(noisy, off by default)" in result.stdout

    def test_missing_file_is_an_error(self, tmp_path: Path):
        result = self.run(str(tmp_path / "nope.md"))
        assert result.returncode == 2
        assert "not a file" in result.stderr


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
