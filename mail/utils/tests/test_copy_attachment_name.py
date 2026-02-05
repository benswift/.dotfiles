"""Tests for attachment filename extraction and lenient parsing."""

from email.message import EmailMessage

from mail_utils.email import (
    get_attachment_filenames,
    get_attachment_filenames_raw,
    read_email_from_bytes_lenient,
)

from mail_utils.cli.copy_attachment_name import _shell_escape


def _make_email_with_attachments(*filenames: str) -> EmailMessage:
    msg = EmailMessage()
    msg["From"] = "sender@example.com"
    msg["To"] = "recipient@example.com"
    msg["Subject"] = "Test"
    msg.set_content("Body text.")
    for name in filenames:
        msg.add_attachment(
            b"fake content",
            maintype="application",
            subtype="octet-stream",
            filename=name,
        )
    return msg


def _strip_toplevel_headers(msg: EmailMessage) -> bytes:
    """Simulate neomutt's <pipe-message> in attach menu: strip top-level headers."""
    raw = msg.as_bytes()
    boundary = msg.get_boundary()
    assert boundary is not None
    marker = b"--" + boundary.encode()
    idx = raw.find(marker)
    return raw[idx:]


class TestGetAttachmentFilenames:
    def test_single_attachment(self):
        msg = _make_email_with_attachments("report.pdf")
        assert get_attachment_filenames(msg) == ["report.pdf"]

    def test_multiple_attachments(self):
        msg = _make_email_with_attachments("a.pdf", "b.docx", "c.png")
        assert get_attachment_filenames(msg) == ["a.pdf", "b.docx", "c.png"]

    def test_no_attachments(self):
        msg = EmailMessage()
        msg["From"] = "sender@example.com"
        msg.set_content("Plain text only.")
        assert get_attachment_filenames(msg) == []

    def test_no_attachments_on_sample_email(self, sample_email: EmailMessage):
        assert get_attachment_filenames(sample_email) == []

    def test_filename_with_spaces(self):
        msg = _make_email_with_attachments("my document (final).pdf")
        assert get_attachment_filenames(msg) == ["my document (final).pdf"]

    def test_filename_with_unicode(self):
        msg = _make_email_with_attachments("résumé.pdf")
        assert get_attachment_filenames(msg) == ["résumé.pdf"]

    def test_inline_parts_without_filename_excluded(self):
        msg = EmailMessage()
        msg["From"] = "sender@example.com"
        msg.set_content("Body.")
        msg.add_related(b"<img>", maintype="image", subtype="png")
        msg.add_attachment(b"data", maintype="application", subtype="pdf", filename="doc.pdf")
        filenames = get_attachment_filenames(msg)
        assert filenames == ["doc.pdf"]


class TestGetAttachmentFilenamesRaw:
    def test_extracts_from_mime_body_without_toplevel_headers(self):
        raw = (
            b'--boundary\r\n'
            b'Content-Type: application/pdf; name="report.pdf"\r\n'
            b'Content-Disposition: attachment; filename="report.pdf"\r\n'
            b'\r\n'
            b'fake pdf content\r\n'
            b'--boundary--\r\n'
        )
        assert get_attachment_filenames_raw(raw) == ["report.pdf"]

    def test_deduplicates_filenames(self):
        raw = (
            b'Content-Type: application/pdf; name="doc.pdf"\r\n'
            b'Content-Disposition: attachment; filename="doc.pdf"\r\n'
        )
        assert get_attachment_filenames_raw(raw) == ["doc.pdf"]

    def test_multiple_attachments(self):
        raw = (
            b'Content-Disposition: attachment; filename="a.pdf"\r\n'
            b'--boundary\r\n'
            b'Content-Disposition: attachment; filename="b.docx"\r\n'
        )
        assert get_attachment_filenames_raw(raw) == ["a.pdf", "b.docx"]

    def test_no_filenames(self):
        raw = b'Content-Type: text/plain\r\n\r\nJust text.\r\n'
        assert get_attachment_filenames_raw(raw) == []

    def test_filename_with_spaces(self):
        raw = b'Content-Disposition: attachment; filename="my doc (1).pdf"\r\n'
        assert get_attachment_filenames_raw(raw) == ["my doc (1).pdf"]

    def test_works_on_full_email_bytes(self):
        msg = _make_email_with_attachments("test.xlsx")
        raw = msg.as_bytes()
        assert get_attachment_filenames_raw(raw) == ["test.xlsx"]


class TestReadEmailFromBytesLenient:
    def test_normal_email_passes_through(self):
        msg = _make_email_with_attachments("test.pdf")
        data = msg.as_bytes()
        parsed = read_email_from_bytes_lenient(data)
        assert get_attachment_filenames(parsed) == ["test.pdf"]

    def test_stripped_headers_single_attachment(self):
        msg = _make_email_with_attachments("report.pdf")
        stripped = _strip_toplevel_headers(msg)
        parsed = read_email_from_bytes_lenient(stripped)
        assert get_attachment_filenames(parsed) == ["report.pdf"]

    def test_stripped_headers_multiple_attachments(self):
        msg = _make_email_with_attachments("a.pdf", "b.docx")
        stripped = _strip_toplevel_headers(msg)
        parsed = read_email_from_bytes_lenient(stripped)
        assert get_attachment_filenames(parsed) == ["a.pdf", "b.docx"]

    def test_stripped_headers_preserves_content(self):
        msg = _make_email_with_attachments("test.bin")
        stripped = _strip_toplevel_headers(msg)
        parsed = read_email_from_bytes_lenient(stripped)
        parts = [(fn, part) for part in parsed.walk() if (fn := part.get_filename())]
        assert len(parts) == 1
        assert parts[0][1].get_payload(decode=True) == b"fake content"

    def test_no_attachments_still_works(self):
        msg = EmailMessage()
        msg["From"] = "sender@example.com"
        msg.set_content("Plain text.")
        parsed = read_email_from_bytes_lenient(msg.as_bytes())
        assert get_attachment_filenames(parsed) == []


class TestShellEscape:
    def test_simple_path_unchanged(self):
        assert _shell_escape("/tmp/report.pdf") == "/tmp/report.pdf"

    def test_spaces_escaped(self):
        assert _shell_escape("/tmp/my file.pdf") == r"/tmp/my\ file.pdf"

    def test_parentheses_escaped(self):
        assert _shell_escape("/tmp/doc (1).pdf") == r"/tmp/doc\ \(1\).pdf"

    def test_multiple_special_chars(self):
        assert _shell_escape("/tmp/it's a [test].pdf") == r"/tmp/it\'s\ a\ \[test\].pdf"

    def test_already_safe_chars(self):
        assert _shell_escape("/tmp/file-name_v2.3.pdf") == "/tmp/file-name_v2.3.pdf"
