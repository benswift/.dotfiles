"""Rewrite a displayed message's Date: header into local time.

neomutt's `$display_filter`. The pager prints Date: exactly as the sender's
client wrote it, and no setting localises it --- the `%[fmt]` expando only
reaches the index and status lines. Only the header block is touched, and a
date that won't parse passes through unchanged, so the worst case is the
message as neomutt would have shown it anyway.

Stdlib only: this runs on every message opened, so startup time matters.
"""

import sys
from datetime import UTC
from email.utils import parsedate_to_datetime

LOCAL_FORMAT = "%a, %d %b %Y %H:%M %Z"


def localise(value: str) -> str | None:
    """`value` as a local-time string, or None if it isn't a parseable date."""
    try:
        when = parsedate_to_datetime(value)
    except (TypeError, ValueError):
        return None
    # RFC 5322's "-0000" means UTC with the origin's zone unknown, which the
    # stdlib returns as a naive datetime.
    if when.tzinfo is None:
        when = when.replace(tzinfo=UTC)
    return when.astimezone().strftime(LOCAL_FORMAT)


def rewrite(text: str) -> str:
    lines = text.splitlines(keepends=True)
    for i, line in enumerate(lines):
        if not line.strip():
            break  # end of headers: a Date: line in the body is quoted text
        name, colon, value = line.partition(":")
        if colon and name.lower() == "date" and (local := localise(value.strip())):
            ending = line[len(line.rstrip("\r\n")) :]
            lines[i] = f"{name}: {local}{ending}"
    return "".join(lines)


def main() -> None:
    # surrogateescape round-trips whatever charset neomutt hands over
    text = sys.stdin.buffer.read().decode("utf-8", "surrogateescape")
    sys.stdout.buffer.write(rewrite(text).encode("utf-8", "surrogateescape"))


if __name__ == "__main__":
    main()
