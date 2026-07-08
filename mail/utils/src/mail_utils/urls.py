"""Extract URLs from email messages.

Parses the raw MIME message and pulls real links out of the HTML (anchor
``href``s, image ``src``s, plus the anchor text as a human-readable label)
using an actual HTML parser, rather than regex-scraping neomutt's decoded
text. Falls back to the plain-text part when there is no HTML.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from email.message import EmailMessage
from html import unescape
from html.parser import HTMLParser


@dataclass
class Link:
    """A URL and its human-readable label (anchor text, alt text, or "")."""

    url: str
    text: str


# Schemes worth handing to a browser. Everything else (cid:, data:, tel:,
# javascript:, fragment-only, ...) is dropped.
_KEEP_RE = re.compile(r"^(https?://|mailto:|www\.)", re.IGNORECASE)

# Bare URLs in plain-text parts. Stops at whitespace and common trailing
# delimiters; trailing sentence punctuation is trimmed separately.
_TEXT_URL_RE = re.compile(
    r"""(?xi)
    (
      https?://[^\s<>"'`\])}]+      # http(s) URLs
      |
      www\.[a-z0-9.-]+[^\s<>"'`\])}]*  # bare www. hostnames
    )
    """
)

# Cheap sniff for HTML hiding in a part labelled text/plain (happens when a
# single HTML part is piped decoded, e.g. from the attach/compose menu).
_HTML_SNIFF_RE = re.compile(r"<\s*(a|html|body|div|table|p)\b|href\s*=", re.IGNORECASE)


class _LinkParser(HTMLParser):
    """Collect links from HTML, using anchor text as each link's label."""

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.links: list[Link] = []
        # Stack of open <a> tags: each entry is [href, [text fragments]].
        self._anchors: list[list] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        d = {k: (v or "") for k, v in attrs}
        if tag == "a":
            self._anchors.append([d.get("href", "").strip(), []])
        elif tag == "area" and d.get("href"):
            self.links.append(Link(d["href"].strip(), d.get("alt", "").strip()))
        elif tag in ("img", "source") and d.get("src"):
            self.links.append(Link(d["src"].strip(), d.get("alt", "").strip()))

    def handle_endtag(self, tag: str) -> None:
        if tag == "a" and self._anchors:
            href, parts = self._anchors.pop()
            if href:
                label = " ".join("".join(parts).split())
                self.links.append(Link(href, label))

    def handle_data(self, data: str) -> None:
        if self._anchors:
            self._anchors[-1][1].append(data)


def _links_from_html(html: str) -> list[Link]:
    parser = _LinkParser()
    parser.feed(html)
    parser.close()
    return parser.links


def _links_from_text(text: str) -> list[Link]:
    links: list[Link] = []
    for match in _TEXT_URL_RE.finditer(text):
        url = unescape(match.group(1)).rstrip(".,;:!?)\"'>")
        links.append(Link(url, ""))
    return links


def _part_text(part: EmailMessage) -> str:
    """Decode a message part to text, tolerant of charset problems."""
    try:
        content = part.get_content()
        if isinstance(content, str):
            return content
    except (LookupError, ValueError):
        pass
    payload = part.get_payload(decode=True) or b""
    charset = part.get_content_charset() or "utf-8"
    return payload.decode(charset, errors="replace")


def _dedupe(links: list[Link]) -> list[Link]:
    """Drop non-browser links and duplicate URLs, preserving first-seen order.

    When the same URL appears both with and without a label, keep the labelled
    one.
    """
    by_url: dict[str, Link] = {}
    order: list[str] = []
    for link in links:
        url = link.url
        if not _KEEP_RE.match(url):
            continue
        if url not in by_url:
            by_url[url] = link
            order.append(url)
        elif not by_url[url].text and link.text:
            by_url[url] = link
    return [by_url[url] for url in order]


def extract_links(msg: EmailMessage) -> list[Link]:
    """Extract browser-openable links from a parsed email message.

    HTML links (which carry anchor text) are preferred; the plain-text part is
    only used when there is no HTML. A text/plain part that actually contains
    HTML is treated as HTML.
    """
    html_links: list[Link] = []
    text_links: list[Link] = []
    for part in msg.walk():
        if part.get_content_maintype() == "multipart" or part.is_attachment():
            continue
        if part.get_content_maintype() != "text":
            continue
        text = _part_text(part)
        if part.get_content_type() == "text/html" or _HTML_SNIFF_RE.search(text):
            html_links.extend(_links_from_html(text))
        else:
            text_links.extend(_links_from_text(text))
    return _dedupe(html_links or text_links)
