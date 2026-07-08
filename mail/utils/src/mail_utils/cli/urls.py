"""Extract URLs from a piped email and pick one (or more) to open.

Reads a raw RFC822 message on stdin (neomutt's ``<pipe-message>`` with
``pipe_decode`` unset), parses out the links, and presents them in fzf:

- Enter opens the selected web link(s) in the default browser, and starts a
  neomutt compose for any selected ``mailto:`` link (so email stays in the
  terminal rather than launching a GUI mail client)
- Tab marks multiple links (multi-select)
- Ctrl-Y copies the selected link(s) to the clipboard instead of opening

The neomutt compose is handed back via a command file that the ``,b`` macro
sources after this command exits (the same pattern as the markdown-compose
macro). The file is always (re)written --- empty when there's nothing for
neomutt to do --- so the macro's ``source`` is a harmless no-op in that case.

Usage from a shell for debugging:

    mail-urls < message.eml          # interactive picker
    mail-urls --print < message.eml  # just list the extracted URLs
"""

import subprocess
import sys
from urllib.parse import unquote

from mail_utils.clipboard import copy_to_clipboard
from mail_utils.email import (
    read_email_from_bytes,
    read_email_from_bytes_lenient,
)
from mail_utils.urls import Link, extract_links

# Sourced by the ,b neomutt macro after this command exits.
NEOMUTT_CMD_FILE = "/tmp/neomutt-urls-commands"


def _parse(data: bytes):
    for parser in (read_email_from_bytes, read_email_from_bytes_lenient):
        try:
            return extract_links(parser(data))
        except Exception:
            continue
    return []


def _write_neomutt_commands(text: str) -> None:
    """Write the follow-up command file the ,b macro sources (best effort)."""
    try:
        with open(NEOMUTT_CMD_FILE, "w") as f:
            f.write(text)
    except OSError:
        pass


def _mailto_address(url: str) -> str:
    """Recipient list from a mailto: URL, minus any ?query and URL-encoding."""
    addr = url[len("mailto:") :].split("?", 1)[0]
    return unquote(addr).strip()


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
    # Reset any stale follow-up from a previous run before doing anything.
    _write_neomutt_commands("")
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
        return

    mailtos = [u for u in urls if u.lower().startswith("mailto:")]
    weblinks = [u for u in urls if not u.lower().startswith("mailto:")]

    if weblinks:
        _open(weblinks)

    # Hand the first mailto back to neomutt to compose in-terminal. The macro
    # sources NEOMUTT_CMD_FILE once, so only one compose can be started.
    if mailtos:
        address = _mailto_address(mailtos[0])
        if address:
            _write_neomutt_commands(f'push "<mail>{address}<enter>"\n')
        if len(mailtos) > 1:
            print(
                f"Composing to {address}; ignored {len(mailtos) - 1} other "
                "mailto link(s).",
                file=sys.stderr,
            )


if __name__ == "__main__":
    main()
