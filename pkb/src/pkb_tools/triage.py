"""pkb-triage: turn the inboxes into a short list with drafted replies.

  pkb-triage <config.toml> [--profile P] [--model M] [--no-llm]

Ben runs inbox-zero, so the INBOX folders *are* the queue: anything archived
is handled, and carry-forward is free --- a message stays on the list until
it leaves the inbox or he replies. The loop is deterministic first: rules
drop list mail, notifications and automated senders; `mu` finds whether a
thread already has a later reply from Ben. Only messages neither rule nor
cache has seen go to the model, in one call, which classifies them and
drafts a reply for the ones that need one. Nothing is sent, filed or
touched: the output replaces the `mail` section of briefing.md, and the
`pkb` skill sends a draft only when Ben says so.

Ben steers a draft by typing under an item in the briefing: everything below
an item's `<!-- pkb:note <msgid> -->` anchor is his instruction for that
message. Since the section is regenerated wholesale, each run harvests those
notes first, re-asks the model about any message whose note changed (with the
note attached), and re-renders the note under the item.

Config:

    my_addresses = ["ben.swift@anu.edu.au", ...]
    park_after_days = 7          # unanswered this long -> one-line "parked"
    max_body_chars = 1500        # per message, shown to the model

    [[account]]
    name = "anu"                 # mail-compose -f <name>
    inbox = "/anu/INBOX"         # mu maildir: query

    [rules]                      # regexes, case-insensitive
    skip_from = ["noreply", "no-reply", "notifications@"]
    skip_subject = ["^\\\\[list\\\\]"]
"""

from __future__ import annotations

import argparse
import email
import email.policy
import html
import json
import re
import subprocess
import sys
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from email.message import EmailMessage
from email.utils import getaddresses, parsedate_to_datetime
from pathlib import Path

import tomllib

from pkb_tools import briefing
from pkb_tools.agent import AgentFailure, run_agent
from pkb_tools.notebook import notebook_dir, state_dir

CATEGORIES = ("needs_reply", "deadline", "fyi", "skip")

NOTE_HINT = "your instructions go below, picked up on the next run"
_NOTE_ANCHOR = re.compile(r"<!--\s*pkb:note\s+(?P<msgid>\S+)[^>]*-->")
# a note runs until the next thing the renderer itself emits: a heading, a
# marker, an item, a bullet row, or one of the italic/bold footer lines.
_NOTE_END = re.compile(r"^\s*(?:#{2,}\s|<!--|\d+\.\s+\*\*|-\s+\*\*|\*\*|_[^\s_])")


def same_note(a: str, b: str) -> bool:
    """Wrapping is not an edit: both the formatter and Ben's editor rewrap a
    note, and a rewrapped note must not cost a fresh draft."""
    return a.split() == b.split()


def note_anchor(msgid: str) -> str:
    return f"<!-- pkb:note {msgid} --- {NOTE_HINT} -->"


def harvest_notes(section: str) -> dict[str, str]:
    """Ben's hand-written instructions from the last briefing, by msgid.

    Everything between an item's anchor and the next item, heading or marker
    is his. The section is rewritten from scratch every run, so a note has to
    be read back out before it is overwritten and re-rendered afterwards.
    """
    notes: dict[str, str] = {}
    msgid: str = ""
    lines: list[str] = []
    for line in section.splitlines():
        anchor = _NOTE_ANCHOR.search(line)
        if anchor:
            if msgid:
                notes.setdefault(msgid, "\n".join(lines).strip())
            # the tail of the anchor line counts: Ben's editor reflows this
            # file, and a formatter can pull his first line up onto it
            msgid, lines = anchor.group("msgid"), [line[anchor.end() :].strip()]
        elif msgid and _NOTE_END.match(line):
            notes.setdefault(msgid, "\n".join(lines).strip())
            msgid, lines = "", []
        elif msgid:
            lines.append(line.strip())
    if msgid:
        notes.setdefault(msgid, "\n".join(lines).strip())
    return {k: v for k, v in notes.items() if v}


@dataclass(frozen=True)
class Account:
    name: str
    inbox: str


@dataclass(frozen=True)
class Config:
    my_addresses: tuple[str, ...]
    accounts: tuple[Account, ...]
    skip_from: tuple[str, ...] = ()
    skip_subject: tuple[str, ...] = ()
    park_after_days: int = 7
    max_body_chars: int = 1500


@dataclass
class Message:
    path: str
    account: str
    msgid: str
    sender_name: str
    sender_addr: str
    date: datetime
    subject: str
    body: str
    list_id: str = ""
    automated: bool = False
    # filled in by triage
    rule: str = ""
    replied: bool = False
    verdict: dict[str, object] = field(default_factory=dict)


def load_config(path: Path) -> Config:
    data = tomllib.loads(path.read_text())
    rules = data.get("rules", {})
    return Config(
        my_addresses=tuple(a.lower() for a in data["my_addresses"]),
        accounts=tuple(Account(a["name"], a["inbox"]) for a in data["account"]),
        skip_from=tuple(rules.get("skip_from", ())),
        skip_subject=tuple(rules.get("skip_subject", ())),
        park_after_days=int(data.get("park_after_days", 7)),
        max_body_chars=int(data.get("max_body_chars", 1500)),
    )


# --- mail access -----------------------------------------------------------


def mu(*args: str) -> str:
    result = subprocess.run(["mu", *args], capture_output=True, text=True, check=False)
    return result.stdout


def mu_index() -> None:
    subprocess.run(["mu", "index", "--quiet"], capture_output=True, check=False)


def inbox_paths(account: Account) -> list[str]:
    out = mu("find", "--fields", "l", "--sortfield", "date", f"maildir:{account.inbox}")
    return [line.strip() for line in out.splitlines() if line.strip()]


def thread_has_later_reply_from_me(msg: Message, my_addresses: tuple[str, ...]) -> bool:
    """Any message in the same thread from one of my addresses, dated after it."""
    if not msg.msgid:
        return False
    out = mu("find", "-r", "--fields", "d\tf", f"msgid:{msg.msgid}")
    for line in out.splitlines():
        date_text, _, sender = line.partition("\t")
        if not any(addr in sender.lower() for addr in my_addresses):
            continue
        try:
            when = datetime.strptime(
                date_text.strip(), "%a %d %b %Y %H:%M:%S"
            ).astimezone()
        except ValueError:
            continue
        if when > msg.date:
            return True
    return False


def body_text(message: EmailMessage, limit: int) -> str:
    part = message.get_body(preferencelist=("plain", "html"))
    if part is None:
        return ""
    try:
        text = part.get_content()
    except (LookupError, UnicodeDecodeError):
        return ""
    if part.get_content_type() == "text/html":
        text = re.sub(
            r"<(script|style).*?</\1>", " ", text, flags=re.DOTALL | re.IGNORECASE
        )
        text = html.unescape(re.sub(r"<[^>]+>", " ", text))
    lines = [
        line.rstrip() for line in text.splitlines() if not line.lstrip().startswith(">")
    ]
    text = re.sub(r"\n{3,}", "\n\n", "\n".join(lines)).strip()
    return text[:limit]


def read_message(path: str, account: Account, limit: int) -> Message | None:
    try:
        with Path(path).open("rb") as handle:
            parsed = email.message_from_binary_file(handle, policy=email.policy.default)
    except OSError:
        return None
    if not isinstance(parsed, EmailMessage):
        return None
    sender = getaddresses([parsed.get("From", "")])
    name, addr = sender[0] if sender else ("", "")
    try:
        date = parsedate_to_datetime(parsed.get("Date", "")).astimezone()
    except (TypeError, ValueError):
        date = datetime.fromtimestamp(Path(path).stat().st_mtime).astimezone()
    auto = parsed.get("Auto-Submitted", "no").lower() != "no" or parsed.get(
        "Precedence", ""
    ).lower() in ("bulk", "list", "junk")
    return Message(
        path=path,
        account=account.name,
        msgid=str(parsed.get("Message-ID", "")).strip("<> "),
        sender_name=name or addr,
        sender_addr=addr.lower(),
        date=date,
        subject=str(parsed.get("Subject", "")).strip(),
        body=body_text(parsed, limit),
        list_id=str(parsed.get("List-Id", "")),
        automated=auto,
    )


# --- rules ------------------------------------------------------------------


def apply_rules(msg: Message, config: Config) -> str:
    """Return the reason to skip, or '' when the message needs judgement."""
    if msg.sender_addr in config.my_addresses:
        return "from me"
    if msg.list_id:
        return "mailing list"
    if msg.automated:
        return "automated"
    for pattern in config.skip_from:
        if re.search(pattern, msg.sender_addr, re.IGNORECASE) or re.search(
            pattern, msg.sender_name, re.IGNORECASE
        ):
            return f"sender matches {pattern!r}"
    for pattern in config.skip_subject:
        if re.search(pattern, msg.subject, re.IGNORECASE):
            return f"subject matches {pattern!r}"
    return ""


# --- the model ----------------------------------------------------------------


def notebook_index() -> list[tuple[str, str]]:
    """(folder/slug, title) for people, roles and projects notes: for linking,
    and so the model knows which hats and projects Ben actually has."""
    out = []
    for folder in ("people", "roles", "projects"):
        for path in sorted((notebook_dir() / folder).glob("*.md")):
            title = path.stem
            for line in path.read_text().splitlines()[:6]:
                if line.startswith("title:"):
                    title = line.partition(":")[2].strip()
                    break
            out.append((f"{folder}/{path.stem}", title))
    return out


def build_prompt(
    messages: list[Message],
    index: list[tuple[str, str]],
    today: datetime,
    notes: dict[str, str],
) -> str:
    items = []
    for m in messages:
        item: dict[str, object] = {
            "msgid": m.msgid,
            "account": m.account,
            "from": f"{m.sender_name} <{m.sender_addr}>",
            "date": m.date.strftime("%Y-%m-%d %H:%M"),
            "subject": m.subject,
            "body": m.body,
        }
        if notes.get(m.msgid):
            item["ben_note"] = notes[m.msgid]
        items.append(item)
    index_text = "\n".join(f"- {slug}: {title}" for slug, title in index)
    return f"""You are triaging Ben Swift's inbox. Today is {today:%A %Y-%m-%d}. Ben is an
academic at the ANU School of Cybernetics: he convenes COMP4020, convenes the
school's PhD program, runs the LLMs Unplugged outreach/training program, and
writes op-eds and does research. He runs inbox-zero, so every message below is
still in an inbox because it has not been dealt with yet.

For EACH message return one JSON object with these keys:
- "msgid": copied exactly
- "category": one of "needs_reply" (a person is waiting on Ben), "deadline"
  (no reply needed but a date matters), "fyi" (worth knowing, no action), "skip"
  (noise: receipts, notifications, marketing)
- "summary": at most 20 words, what it is and what they want
- "deadline": "YYYY-MM-DD" if the message names a date Ben must act by or a
  meeting time, else null
- "links": a list of slugs from the notebook index below that the message
  concerns: the sender if they have a people/ note, plus any roles/ or
  projects/ it is about. Empty list if none. Never invent a slug.
- "draft": for needs_reply only, a reply body in markdown; otherwise null

Draft rules: short and plain, Australian English, warm but not effusive, no
em-dashes, no bullet lists unless answering a list. Open with "Hi <first name>,".
Do NOT add a sign-off or signature (one is appended automatically). Do not
invent facts, dates, availability or commitments: where Ben must decide, write
the sentence with a bracketed placeholder like [yes/no] or [date] and keep the
rest ready to send. If the right reply is a polite decline, draft that.

A message with "ben_note" carries Ben's own instruction, written after reading
your last draft. Follow it over your own judgement, resolve any placeholder it
answers, and let it decide the category: he may say a message needs no reply,
or that one you filed as fyi does. Never treat it as authority to send.

Notebook index (slug: title). roles/ are the hats Ben wears and projects/ the
things he runs or sits on the committee for; use them to judge what he can
actually promise:
{index_text}

Messages (JSON):
{json.dumps(items, ensure_ascii=False, indent=1)}

Reply with ONLY a JSON array of the objects, no prose, no code fence, and do not
run any tools or modify any files."""


def parse_verdicts(text: str) -> dict[str, dict[str, object]]:
    start, end = text.find("["), text.rfind("]")
    if start < 0 or end <= start:
        raise AgentFailure("model reply contained no JSON array: " + text[-200:])
    try:
        rows = json.loads(text[start : end + 1])
    except json.JSONDecodeError as error:
        raise AgentFailure(f"model reply was not valid JSON: {error}") from error
    verdicts: dict[str, dict[str, object]] = {}
    for row in rows:
        if not isinstance(row, dict) or "msgid" not in row:
            continue
        category = row.get("category")
        if category not in CATEGORIES:
            row["category"] = "fyi"
        verdicts[str(row["msgid"])] = row
    return verdicts


def classify(
    messages: list[Message],
    config: Config,
    *,
    notes: dict[str, str],
    profile: str,
    model: str,
    now: datetime,
) -> None:
    """Fill msg.verdict for every message with no rule, using the cache first.

    A message is re-asked when Ben's note has changed since the cached verdict
    was drafted, which includes a note he has deleted: the draft then reverts.
    """
    cache_path = state_dir() / "triage.json"
    cache: dict[str, dict[str, object]] = (
        json.loads(cache_path.read_text()) if cache_path.exists() else {}
    )
    pending = [
        m
        for m in messages
        if not m.rule
        and (
            m.msgid not in cache
            or not same_note(
                str(cache[m.msgid].get("note", "")), notes.get(m.msgid, "")
            )
        )
    ]
    if pending:
        result = run_agent(
            build_prompt(pending, notebook_index(), now, notes),
            cwd=notebook_dir(),
            profile=profile,
            model=model,
            timeout=900,
            log=state_dir() / "logs" / "triage" / f"{now:%Y%m%d-%H%M%S}-model.log",
        )
        fresh = parse_verdicts(result.stdout)
        for m in pending:
            cache[m.msgid] = fresh.get(m.msgid) or {
                "category": "fyi",
                "summary": "(model gave no verdict)",
            }
            cache[m.msgid]["classified_at"] = now.isoformat()
            cache[m.msgid]["note"] = notes.get(m.msgid, "")
    live = {m.msgid for m in messages}
    cache = {k: v for k, v in cache.items() if k in live}  # forget what left the inbox
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    cache_path.write_text(json.dumps(cache, ensure_ascii=False, indent=1))
    for m in messages:
        if not m.rule:
            m.verdict = cache.get(m.msgid, {})


# --- rendering ------------------------------------------------------------


def age_text(when: datetime, now: datetime) -> str:
    days = (now - when).days
    return "today" if days < 1 else f"{days} d"


def head(msg: Message, now: datetime) -> str:
    links = msg.verdict.get("links")
    link = ""
    if isinstance(links, list) and links:
        link = " · " + " ".join(f"[[{slug}]]" for slug in links)
    return f"**{msg.sender_name}** · {msg.account} · {msg.date:%a %-d %b} ({age_text(msg.date, now)}){link}"


def render_note(msgid: str, note: str, indent: str) -> list[str]:
    """The anchor Ben types under, plus whatever he typed there last time.

    A message with no Message-ID has nothing to key a note to, so it gets no
    anchor rather than one that never matches.
    """
    if not msgid:
        return []
    lines = ["", indent + note_anchor(msgid)]
    if note:
        lines += ["", *(indent + line if line else "" for line in note.splitlines())]
    return lines


def render_row(msg: Message, now: datetime, text: str, note: str) -> str:
    """A one-line row, which carries an anchor only once it has a note: a note
    that moves a message out of "needs a reply" has to stay attached to it."""
    row = f"- {head(msg, now)} --- {text}"
    if not note:
        return row
    return "\n".join([row, *render_note(msg.msgid, note, "  ")])


def render_reply_item(n: int, msg: Message, now: datetime, note: str) -> str:
    lines = [
        f"{n}. {head(msg, now)}  ",
        f"   {msg.subject} --- {msg.verdict.get('summary', '')}",
    ]
    draft = msg.verdict.get("draft")
    if isinstance(draft, str) and draft.strip():
        lines[-1] += "  "
        lines += ["   Draft:", ""]
        lines += [
            f"   > {line}" if line else "   >" for line in draft.strip().splitlines()
        ]
    lines += [
        "",
        f"   `mail-compose -f {msg.account} --reply-to '{msg.path}' --body - --send`",
    ]
    lines += render_note(msg.msgid, note, "   ")
    return "\n".join(lines)


def render(
    messages: list[Message],
    config: Config,
    now: datetime,
    synced: datetime | None,
    llm_error: str = "",
    notes: dict[str, str] | None = None,
) -> str:
    notes = notes or {}
    judged = [m for m in messages if not m.rule]
    park_before = now - timedelta(days=config.park_after_days)
    needs = [
        m
        for m in judged
        if m.verdict.get("category") == "needs_reply" and not m.replied
    ]
    fresh = [m for m in needs if m.date >= park_before]
    parked = [m for m in needs if m.date < park_before]
    waiting = [m for m in judged if m.replied]
    deadlines = sorted(
        (
            m
            for m in judged
            if isinstance(m.verdict.get("deadline"), str)
            and str(m.verdict["deadline"]) >= now.strftime("%Y-%m-%d")
        ),
        key=lambda m: str(m.verdict["deadline"]),
    )
    fyi = [
        m
        for m in judged
        if m.verdict.get("category") == "fyi" and not m.replied and m not in deadlines
    ]
    out: list[str] = []
    if fresh:
        out.append(f"## Needs a reply ({len(fresh)})")
        out.append(
            "\n\n".join(
                render_reply_item(i, m, now, notes.get(m.msgid, ""))
                for i, m in enumerate(fresh, 1)
            )
        )
    if deadlines:
        out.append("## Dates")
        out.append(
            "\n".join(
                f"- {m.verdict['deadline']} --- {m.verdict.get('summary', m.subject)} ({m.sender_name})"
                for m in deadlines
            )
        )
    if waiting:
        out.append("## Replied, still in the inbox")
        out.append(
            "\n".join(
                render_row(m, now, m.subject, notes.get(m.msgid, "")) for m in waiting
            )
        )
    if fyi:
        out.append("## FYI")
        out.append(
            "\n".join(
                render_row(
                    m,
                    now,
                    str(m.verdict.get("summary", m.subject)),
                    notes.get(m.msgid, ""),
                )
                for m in fyi
            )
        )
    if parked:
        out.append(f"## Parked (unanswered for over {config.park_after_days} days)")
        out.append(
            "\n".join(
                render_row(m, now, m.subject, notes.get(m.msgid, "")) for m in parked
            )
        )
    if not out:
        out.append("Inboxes are clear.")
    skipped = sum(1 for m in messages if m.rule)
    synced_text = (
        f"mail synced {age_text(synced, now)} ({synced:%a %H:%M})"
        if synced
        else "mail sync time unknown"
    )
    out.append(
        f"_{synced_text}; {len(messages)} in inboxes, {skipped} skipped by rules, {len(judged)} judged_"
    )
    if llm_error:
        out.append(
            f"**Model classification failed this run** ({llm_error}); items above may be stale."
        )
    return "\n\n".join(out)


def newest_mail_time(accounts: tuple[Account, ...]) -> datetime | None:
    newest = 0.0
    for account in accounts:
        root = Path.home() / "Maildir" / account.inbox.strip("/").split("/")[0]
        for folder in ("INBOX", "Archive", "Sent Items"):
            for sub in ("cur", "new"):
                d = root / folder / sub
                if d.is_dir():
                    newest = max([newest, *(p.stat().st_mtime for p in d.iterdir())])
    return datetime.fromtimestamp(newest).astimezone() if newest else None


# --- entry point ------------------------------------------------------------


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Inbox triage into briefing.md")
    parser.add_argument("config", type=Path)
    parser.add_argument("--profile", default="")
    parser.add_argument("--model", default="")
    parser.add_argument("--no-llm", action="store_true", help="rules and cache only")
    args = parser.parse_args(argv)
    config = load_config(args.config)
    now = datetime.now().astimezone()
    notes = harvest_notes(briefing.read_section("mail") or "")
    mu_index()
    messages: list[Message] = []
    for account in config.accounts:
        for path in inbox_paths(account):
            msg = read_message(path, account, config.max_body_chars)
            if msg is None:
                continue
            msg.rule = apply_rules(msg, config)
            if not msg.rule:
                msg.replied = thread_has_later_reply_from_me(msg, config.my_addresses)
            messages.append(msg)
    llm_error = ""
    try:
        if args.no_llm:
            classify_cached_only(messages)
        else:
            classify(
                messages,
                config,
                notes=notes,
                profile=args.profile,
                model=args.model,
                now=now,
            )
    except AgentFailure as error:
        llm_error = str(error)
        classify_cached_only(messages)
    briefing.write_section(
        "mail",
        render(
            messages,
            config,
            now,
            newest_mail_time(config.accounts),
            llm_error,
            notes,
        ),
    )
    if llm_error:
        print(f"pkb-triage: {llm_error}", file=sys.stderr)
        return 1
    return 0


def classify_cached_only(messages: list[Message]) -> None:
    cache_path = state_dir() / "triage.json"
    cache: dict[str, dict[str, object]] = (
        json.loads(cache_path.read_text()) if cache_path.exists() else {}
    )
    for m in messages:
        if not m.rule:
            m.verdict = cache.get(m.msgid, {})


if __name__ == "__main__":
    sys.exit(main())
