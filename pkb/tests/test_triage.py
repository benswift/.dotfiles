from __future__ import annotations

import json
from datetime import datetime, timedelta
from email.message import EmailMessage
from email.utils import format_datetime
from pathlib import Path

import pytest
from pkb_tools import triage
from pkb_tools.agent import AgentFailure, AgentResult

NOW = datetime(2026, 9, 7, 6, 30).astimezone()
CONFIG_TEXT = """
my_addresses = ["ben@example.edu"]
park_after_days = 7
[[account]]
name = "anu"
inbox = "/anu/INBOX"
[rules]
skip_from = ["noreply", "notifications@"]
skip_subject = ["digest"]
"""


def write_mail(
    folder: Path,
    *,
    sender: str,
    subject: str,
    body: str,
    when: datetime,
    msgid: str,
    headers: dict[str, str] | None = None,
) -> str:
    msg = EmailMessage()
    msg["From"] = sender
    msg["To"] = "Ben Swift <ben@example.edu>"
    msg["Subject"] = subject
    msg["Date"] = format_datetime(when)
    msg["Message-ID"] = f"<{msgid}>"
    for key, value in (headers or {}).items():
        msg[key] = value
    msg.set_content(body)
    folder.mkdir(parents=True, exist_ok=True)
    path = folder / f"{msgid}.eml"
    path.write_bytes(bytes(msg))
    return str(path)


@pytest.fixture
def config(tmp_path: Path) -> triage.Config:
    path = tmp_path / "triage.toml"
    path.write_text(CONFIG_TEXT)
    return triage.load_config(path)


@pytest.fixture
def inbox(tmp_path: Path) -> Path:
    return tmp_path / "Maildir" / "anu" / "INBOX" / "cur"


def test_rules_skip_lists_bots_and_me(config: triage.Config, inbox: Path) -> None:
    account = config.accounts[0]
    cases = {
        "list": write_mail(
            inbox,
            sender="a@b.c",
            subject="hi",
            body="x",
            when=NOW,
            msgid="l",
            headers={"List-Id": "<foo.list>"},
        ),
        "bot": write_mail(
            inbox,
            sender="notifications@github.com",
            subject="hi",
            body="x",
            when=NOW,
            msgid="b",
        ),
        "auto": write_mail(
            inbox,
            sender="a@b.c",
            subject="hi",
            body="x",
            when=NOW,
            msgid="a",
            headers={"Auto-Submitted": "auto-generated"},
        ),
        "me": write_mail(
            inbox, sender="ben@example.edu", subject="hi", body="x", when=NOW, msgid="m"
        ),
        "digest": write_mail(
            inbox,
            sender="a@b.c",
            subject="Weekly digest",
            body="x",
            when=NOW,
            msgid="d",
        ),
        "human": write_mail(
            inbox,
            sender="Ada <ada@b.c>",
            subject="Question",
            body="Can you?",
            when=NOW,
            msgid="h",
        ),
    }
    verdicts = {}
    for name, path in cases.items():
        msg = triage.read_message(path, account, 500)
        assert msg is not None
        verdicts[name] = triage.apply_rules(msg, config)
    assert verdicts["human"] == ""
    assert all(verdicts[k] for k in ("list", "bot", "auto", "me", "digest"))


def test_body_drops_quoted_lines_and_truncates(
    config: triage.Config, inbox: Path
) -> None:
    path = write_mail(
        inbox,
        sender="a@b.c",
        subject="s",
        body="new line\n> quoted\n" + "x" * 100,
        when=NOW,
        msgid="q",
    )
    msg = triage.read_message(path, config.accounts[0], 20)
    assert msg is not None
    assert "quoted" not in msg.body and len(msg.body) <= 20


def test_later_reply_from_me_detected(
    monkeypatch: pytest.MonkeyPatch, config: triage.Config, inbox: Path
) -> None:
    path = write_mail(
        inbox,
        sender="Ada <ada@b.c>",
        subject="s",
        body="b",
        when=NOW - timedelta(days=1),
        msgid="t1",
    )
    msg = triage.read_message(path, config.accounts[0], 500)
    assert msg is not None
    later = (NOW - timedelta(hours=3)).strftime("%a %d %b %Y %H:%M:%S")
    earlier = (NOW - timedelta(days=2)).strftime("%a %d %b %Y %H:%M:%S")
    monkeypatch.setattr(
        triage,
        "mu",
        lambda *_a: (
            f"{earlier}\tBen Swift <ben@example.edu>\n{later}\tBen Swift <ben@example.edu>\n"
        ),
    )
    assert triage.thread_has_later_reply_from_me(msg, config.my_addresses)
    monkeypatch.setattr(
        triage, "mu", lambda *_a: f"{earlier}\tBen Swift <ben@example.edu>\n"
    )
    assert not triage.thread_has_later_reply_from_me(msg, config.my_addresses)


def test_parse_verdicts_tolerates_prose_and_bad_category() -> None:
    text = 'Sure, here you go:\n[{"msgid": "a", "category": "needs_reply", "draft": "Hi"}, {"msgid": "b", "category": "nonsense"}]\nDone.'
    verdicts = triage.parse_verdicts(text)
    assert verdicts["a"]["draft"] == "Hi"
    assert verdicts["b"]["category"] == "fyi"
    with pytest.raises(AgentFailure):
        triage.parse_verdicts("no json here")


def test_classify_uses_cache_and_only_asks_about_new(
    notebook: Path, monkeypatch: pytest.MonkeyPatch, config: triage.Config, inbox: Path
) -> None:
    account = config.accounts[0]
    old = triage.read_message(
        write_mail(
            inbox,
            sender="Ada <ada@b.c>",
            subject="old",
            body="b",
            when=NOW,
            msgid="old",
        ),
        account,
        500,
    )
    new = triage.read_message(
        write_mail(
            inbox,
            sender="Bob <bob@b.c>",
            subject="new",
            body="b",
            when=NOW,
            msgid="new",
        ),
        account,
        500,
    )
    assert old and new
    cache = notebook.parent / "state" / "triage.json"
    cache.parent.mkdir()
    cache.write_text(
        json.dumps(
            {
                "old": {"category": "fyi", "summary": "cached"},
                "gone": {"category": "skip"},
            }
        )
    )
    asked: list[str] = []

    def fake_agent(prompt: str, **_k: object) -> AgentResult:
        asked.append(prompt)
        return AgentResult(
            '[{"msgid": "new", "category": "needs_reply", "summary": "asks", "draft": "Hi Bob,\\n\\nSure."}]',
            "",
            0,
        )

    monkeypatch.setattr(triage, "run_agent", fake_agent)
    triage.classify([old, new], config, notes={}, profile="", model="", now=NOW)
    assert len(asked) == 1 and '"old"' not in asked[0] and '"new"' in asked[0]
    assert old.verdict["summary"] == "cached"
    assert new.verdict["category"] == "needs_reply"
    saved = json.loads(cache.read_text())
    assert set(saved) == {"old", "new"}  # 'gone' left the inbox, so forgotten


def test_render_sections(config: triage.Config, inbox: Path) -> None:
    account = config.accounts[0]
    fresh = triage.read_message(
        write_mail(
            inbox,
            sender="Ada <ada@b.c>",
            subject="Workshop?",
            body="b",
            when=NOW - timedelta(days=2),
            msgid="f",
        ),
        account,
        500,
    )
    stale = triage.read_message(
        write_mail(
            inbox,
            sender="Old <old@b.c>",
            subject="Ancient",
            body="b",
            when=NOW - timedelta(days=30),
            msgid="s",
        ),
        account,
        500,
    )
    answered = triage.read_message(
        write_mail(
            inbox,
            sender="Eve <eve@b.c>",
            subject="Thanks",
            body="b",
            when=NOW - timedelta(days=1),
            msgid="r",
        ),
        account,
        500,
    )
    bot = triage.read_message(
        write_mail(
            inbox,
            sender="noreply@x.y",
            subject="Receipt",
            body="b",
            when=NOW,
            msgid="n",
        ),
        account,
        500,
    )
    assert fresh and stale and answered and bot
    fresh.verdict = {
        "category": "needs_reply",
        "summary": "wants a workshop",
        "draft": "Hi Ada,\n\nYes [date].",
        "deadline": "2026-09-30",
        "links": ["people/ada-lovelace", "projects/acsw27"],
    }
    stale.verdict = {"category": "needs_reply", "summary": "old ask"}
    answered.verdict = {"category": "needs_reply", "summary": "x"}
    answered.replied = True
    bot.rule = "sender matches"
    text = triage.render(
        [fresh, stale, answered, bot], config, NOW, NOW - timedelta(minutes=40)
    )
    assert "## Needs a reply (1)" in text
    assert "> Hi Ada," in text and "[[people/ada-lovelace]] [[projects/acsw27]]" in text
    assert "\n\n\n" not in text
    assert f"mail-compose -f anu --reply-to '{fresh.path}' --body - --send" in text
    assert "## Dates" in text and "2026-09-30" in text
    assert "## Replied, still in the inbox" in text and "Eve" in text
    assert "## Parked" in text and "Ancient" in text
    assert "4 in inboxes, 1 skipped by rules, 3 judged" in text


def test_render_empty_inbox(config: triage.Config) -> None:
    assert "Inboxes are clear." in triage.render([], config, NOW, None)


def test_harvest_notes_reads_what_ben_typed_under_the_anchor() -> None:
    section = """## Needs a reply (2)

1. **Ada** · anu · Mon 7 Sep (today)
   Workshop? --- wants a workshop
   Draft:
   > Hi Ada,
   `mail-compose -f anu --reply-to '/x' --body - --send`
   <!-- pkb:note a@b.c --- your instructions go below, picked up on the next run -->
   Say yes, Thursday 2pm at the school.
   Ask him to bring the gradebook demo.

2. **Bob** · anu · Mon 7 Sep (today)
   Hello --- says hi
   `mail-compose -f anu --reply-to '/y' --body - --send`
   <!-- pkb:note b@b.c --- your instructions go below, picked up on the next run -->

## FYI

- **Eve** · anu · Mon 7 Sep (today) --- a thing
  <!-- pkb:note e@b.c --- your instructions go below, picked up on the next run -->
  This one does need an answer after all.

<!-- /pkb:mail -->"""
    notes = triage.harvest_notes(section)
    assert notes == {
        "a@b.c": "Say yes, Thursday 2pm at the school.\nAsk him to bring the gradebook demo.",
        "e@b.c": "This one does need an answer after all.",
    }
    assert triage.harvest_notes("") == {}


def test_harvest_notes_survives_a_reflowed_file() -> None:
    """Ben edits briefing.md in Helix, which reformats markdown on save."""
    section = (
        "1. **Ada** · anu · Mon 7 Sep (today) Workshop? --- wants one\n"
        "<!-- pkb:note a@b.c --- hint --> Say yes, Thursday 2pm.\n"
        "_mail synced today (Mon 06:30); 1 in inboxes, 0 skipped by rules, 1 judged_\n"
    )
    assert triage.harvest_notes(section) == {"a@b.c": "Say yes, Thursday 2pm."}


def test_rendered_notes_round_trip(config: triage.Config, inbox: Path) -> None:
    account = config.accounts[0]
    fresh = triage.read_message(
        write_mail(
            inbox,
            sender="Ada <a@b.c>",
            subject="Workshop?",
            body="b",
            when=NOW,
            msgid="f",
        ),
        account,
        500,
    )
    told = triage.read_message(
        write_mail(
            inbox, sender="Eve <e@b.c>", subject="Thanks", body="b", when=NOW, msgid="i"
        ),
        account,
        500,
    )
    assert fresh and told
    fresh.verdict = {"category": "needs_reply", "summary": "wants a workshop"}
    told.verdict = {"category": "fyi", "summary": "a thing"}
    notes = {"f": "Say yes, Thursday 2pm.", "i": "Reply after all."}
    text = triage.render([fresh, told], config, NOW, None, "", notes)
    assert "Say yes, Thursday 2pm." in text and "<!-- pkb:note f " in text
    assert triage.harvest_notes(text) == notes
    # an un-noted one-line row stays a one-liner
    plain = triage.render([fresh, told], config, NOW, None, "", {})
    assert "<!-- pkb:note i " not in plain and "<!-- pkb:note f " in plain
    fresh.msgid = ""  # nothing to key a note to
    assert "pkb:note" not in triage.render([fresh], config, NOW, None, "", {})


def test_a_changed_note_reasks_the_model(
    notebook: Path, monkeypatch: pytest.MonkeyPatch, config: triage.Config, inbox: Path
) -> None:
    account = config.accounts[0]
    msg = triage.read_message(
        write_mail(
            inbox,
            sender="Ada <a@b.c>",
            subject="Workshop?",
            body="b",
            when=NOW,
            msgid="f",
        ),
        account,
        500,
    )
    assert msg
    cache = notebook.parent / "state" / "triage.json"
    cache.parent.mkdir()
    cache.write_text(
        json.dumps({"f": {"category": "needs_reply", "draft": "Hi", "note": ""}})
    )
    asked: list[str] = []

    def fake_agent(prompt: str, **_k: object) -> AgentResult:
        asked.append(prompt)
        return AgentResult(
            '[{"msgid": "f", "category": "needs_reply", "summary": "s", "draft": "Yes, Thursday."}]',
            "",
            0,
        )

    monkeypatch.setattr(triage, "run_agent", fake_agent)
    notes = {"f": "Say yes, Thursday 2pm."}
    triage.classify([msg], config, notes=notes, profile="", model="", now=NOW)
    assert len(asked) == 1 and "Say yes, Thursday 2pm." in asked[0]
    assert msg.verdict["draft"] == "Yes, Thursday."
    assert json.loads(cache.read_text())["f"]["note"] == notes["f"]

    triage.classify([msg], config, notes=notes, profile="", model="", now=NOW)
    assert len(asked) == 1  # unchanged note: cache still stands

    triage.classify([msg], config, notes={}, profile="", model="", now=NOW)
    assert len(asked) == 2  # deleted note: redraft without it
