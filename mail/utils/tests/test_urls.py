"""Tests for URL extraction from email messages."""

from email.message import EmailMessage

from mail_utils.urls import extract_links


def _html_msg(html: str) -> EmailMessage:
    msg = EmailMessage()
    msg["Subject"] = "html test"
    msg.set_content("plain fallback")
    msg.add_alternative(html, subtype="html")
    return msg


class TestExtractFromHtml:
    def test_extracts_href_and_anchor_text(self):
        msg = _html_msg('<p>See <a href="https://example.com/page">the page</a></p>')
        links = extract_links(msg)
        assert len(links) == 1
        assert links[0].url == "https://example.com/page"
        assert links[0].text == "the page"

    def test_decodes_entities_in_href(self):
        msg = _html_msg('<a href="https://example.com/?a=1&amp;b=2">x</a>')
        links = extract_links(msg)
        assert links[0].url == "https://example.com/?a=1&b=2"

    def test_prefers_html_over_plain_alternative(self):
        # The plain fallback contains a different URL; HTML should win.
        msg = EmailMessage()
        msg.set_content("plain http://plain.example.com/only")
        msg.add_alternative(
            '<a href="https://html.example.com/only">link</a>', subtype="html"
        )
        urls = [link.url for link in extract_links(msg)]
        assert urls == ["https://html.example.com/only"]

    def test_collapses_whitespace_in_label(self):
        msg = _html_msg('<a href="https://example.com">click\n   here  now</a>')
        assert extract_links(msg)[0].text == "click here now"

    def test_skips_cid_and_fragment_and_javascript(self):
        msg = _html_msg(
            '<a href="cid:abc123">img</a>'
            '<a href="#top">top</a>'
            '<a href="javascript:void(0)">js</a>'
            '<a href="https://real.example.com">real</a>'
        )
        urls = [link.url for link in extract_links(msg)]
        assert urls == ["https://real.example.com"]

    def test_keeps_mailto(self):
        msg = _html_msg('<a href="mailto:hi@example.com">email us</a>')
        assert extract_links(msg)[0].url == "mailto:hi@example.com"

    def test_extracts_image_src_with_alt_label(self):
        msg = _html_msg('<img src="https://cdn.example.com/logo.png" alt="Logo">')
        links = extract_links(msg)
        assert links[0].url == "https://cdn.example.com/logo.png"
        assert links[0].text == "Logo"


class TestExtractFromPlainText:
    def test_extracts_bare_urls(self):
        msg = EmailMessage()
        msg.set_content("Visit https://example.com/a and http://example.org/b today")
        urls = [link.url for link in extract_links(msg)]
        assert urls == ["https://example.com/a", "http://example.org/b"]

    def test_trims_trailing_punctuation(self):
        msg = EmailMessage()
        msg.set_content("Go to https://example.com/page, please.")
        assert extract_links(msg)[0].url == "https://example.com/page"

    def test_extracts_www_hosts(self):
        msg = EmailMessage()
        msg.set_content("see www.example.com for details")
        assert extract_links(msg)[0].url == "www.example.com"


class TestDedup:
    def test_dedupes_repeated_url_keeping_label(self):
        msg = _html_msg(
            '<a href="https://example.com">first</a>'
            '<a href="https://example.com"></a>'
            '<a href="https://example.com">later</a>'
        )
        links = extract_links(msg)
        assert len(links) == 1
        assert links[0].text == "first"

    def test_preserves_first_seen_order(self):
        msg = _html_msg(
            '<a href="https://b.example.com">b</a><a href="https://a.example.com">a</a>'
        )
        urls = [link.url for link in extract_links(msg)]
        assert urls == ["https://b.example.com", "https://a.example.com"]


class TestHtmlInPlainPart:
    def test_html_labelled_as_plain_is_parsed_as_html(self):
        # Mimics a single HTML part piped decoded with no HTML content-type.
        msg = EmailMessage()
        msg.set_content(
            '<html><body><a href="https://example.com">hi</a></body></html>'
        )
        links = extract_links(msg)
        assert links[0].url == "https://example.com"
        assert links[0].text == "hi"
