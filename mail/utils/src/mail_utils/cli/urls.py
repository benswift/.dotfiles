"""Extract URLs from a piped email and pick one (or more) to open.

Reads a raw RFC822 message on stdin (neomutt's ``<pipe-message>`` with
``pipe_decode`` unset), parses out the links, and presents them in fzf:

- Enter opens the selected link(s) in the default browser
- Tab marks multiple links (multi-select)
- Ctrl-Y copies the selected link(s) to the clipboard instead of opening

Usage from a shell for debugging:

    mail-urls < message.eml          # interactive picker
    mail-urls --print < message.eml  # just list the extracted URLs
"""

import subprocess
import sys

from mail_utils.clipboard import copy_to_clipboard
from mail_utils.email import (
    read_email_from_bytes,
    read_email_from_bytes_lenient,
)
from mail_utils.urls import Link, extract_links


def _parse(data: bytes):
    for parser in (read_email_from_bytes, read_email_from_bytes_lenient):
        try:
            return extract_links(parser(data))
        except Exception:
            continue
    return []


def _display(link: Link) -> str:
    """One fzf row: visible label + URL, with the raw URL as a hidden field."""
    if link.text and link.text != link.url:
        shown = f"{link.text}  \033[2m{link.url}\033[0m"
    else:
        shown = link.url
    # The real URL is appended after a tab and hidden from display via
    # --with-nth=1, so it survives label truncation and ANSI styling.
    return f"{shown}\t{link.url}"


def _pick(links: list[Link]) -> tuple[str, list[str]]:
    """Run fzf. Returns (key, urls) where key is "" for Enter or "ctrl-y"."""
    lines = "\n".join(_display(link) for link in links)
    result = subprocess.run(
        [
            "fzf",
            "--ansi",
            "--multi",
            "--no-sort",
            "--layout=reverse",
            "--height=100%",
            "--info=inline",
            "--delimiter=\t",
            "--with-nth=1",
            "--prompt=open url> ",
            "--header=enter: open  ·  tab: multi-select  ·  ctrl-y: copy",
            "--expect=ctrl-y",
        ],
        input=lines,
        capture_output=True,
        text=True,
    )
    # 130 = cancelled with Esc/Ctrl-C; anything non-zero means no selection.
    if result.returncode not in (0, 1):
        return ("", [])
    out = result.stdout.splitlines()
    if not out:
        return ("", [])
    key, rows = out[0], out[1:]
    urls = [row.split("\t")[-1] for row in rows if row]
    return (key, urls)


def _open(urls: list[str]) -> None:
    opener = "open" if sys.platform == "darwin" else "xdg-open"
    for url in urls:
        target = f"https://{url}" if url.lower().startswith("www.") else url
        subprocess.run([opener, target], check=False)


def main() -> None:
    data = sys.stdin.buffer.read()
    links = _parse(data)

    if not links:
        print("No links found in message.", file=sys.stderr)
        return

    if "--print" in sys.argv[1:]:
        for link in links:
            print(link.url)
        return

    key, urls = _pick(links)
    if not urls:
        return

    if key == "ctrl-y":
        copy_to_clipboard("\n".join(urls))
        print(f"Copied {len(urls)} link(s) to clipboard.", file=sys.stderr)
    else:
        _open(urls)


if __name__ == "__main__":
    main()
