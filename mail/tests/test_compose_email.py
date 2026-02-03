#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "typer"]
# ///
"""Tests for compose-email script."""

import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "bin"))

from importlib.machinery import SourceFileLoader
from importlib.util import module_from_spec, spec_from_loader

loader = SourceFileLoader(
    "compose_email",
    str(Path(__file__).parent.parent.parent / "bin/compose-email"),
)
spec = spec_from_loader("compose_email", loader)
assert spec is not None
compose_email = module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(compose_email)

substitute_template = compose_email.substitute_template
filter_records = compose_email.filter_records
build_email = compose_email.build_email


class TestSubstituteTemplate:
    def test_simple_substitution(self):
        template = "Hello {{name}}"
        data = {"name": "Alice"}
        assert substitute_template(template, data) == "Hello Alice"

    def test_multiple_fields(self):
        template = "Dear {{name}}, your ID is {{id}}"
        data = {"name": "Bob", "id": 12345}
        assert substitute_template(template, data) == "Dear Bob, your ID is 12345"

    def test_missing_field_preserved(self):
        template = "Hello {{name}}, your {{unknown}} field"
        data = {"name": "Carol"}
        assert substitute_template(template, data) == "Hello Carol, your {{unknown}} field"

    def test_list_field_joined(self):
        template = "Panel: {{supervisory_panel}}"
        data = {"supervisory_panel": ["Alice", "Bob", "Carol"]}
        assert substitute_template(template, data) == "Panel: Alice, Bob, Carol"

    def test_empty_list(self):
        template = "Panel: {{supervisory_panel}}"
        data = {"supervisory_panel": []}
        assert substitute_template(template, data) == "Panel: "

    def test_numeric_field(self):
        template = "ID: {{id}}"
        data = {"id": 7654321}
        assert substitute_template(template, data) == "ID: 7654321"

    def test_email_address_template(self):
        template = "u{{id}}@anu.edu.au"
        data = {"id": 1234567}
        assert substitute_template(template, data) == "u1234567@anu.edu.au"

    def test_complex_template(self):
        template = """Hi {{preferred_name}},

Your supervisor {{primary_supervisor}} will be in touch.

Panel members: {{supervisory_panel}}"""
        data = {
            "preferred_name": "Danny",
            "primary_supervisor": "Alex Zafiroglu",
            "supervisory_panel": ["Katherine Daniell", "Amy McLennan"],
        }
        expected = """Hi Danny,

Your supervisor Alex Zafiroglu will be in touch.

Panel members: Katherine Daniell, Amy McLennan"""
        assert substitute_template(template, data) == expected


class TestFilterRecords:
    def test_filter_by_status(self):
        records = [
            {"name": "Alice", "status": "active"},
            {"name": "Bob", "status": "paused"},
            {"name": "Carol", "status": "active"},
        ]
        result = filter_records(records, 'status == "active"')
        assert len(result) == 2
        assert result[0]["name"] == "Alice"
        assert result[1]["name"] == "Carol"

    def test_filter_no_match(self):
        records = [
            {"name": "Alice", "status": "active"},
            {"name": "Bob", "status": "active"},
        ]
        result = filter_records(records, 'status == "paused"')
        assert len(result) == 0

    def test_filter_all_match(self):
        records = [
            {"name": "Alice", "status": "active"},
            {"name": "Bob", "status": "active"},
        ]
        result = filter_records(records, 'status == "active"')
        assert len(result) == 2

    def test_empty_filter_returns_all(self):
        records = [{"name": "Alice"}, {"name": "Bob"}]
        result = filter_records(records, "")
        assert len(result) == 2

    def test_none_filter_returns_all(self):
        records = [{"name": "Alice"}, {"name": "Bob"}]
        result = filter_records(records, None)
        assert len(result) == 2

    def test_filter_by_numeric_comparison(self):
        records = [
            {"name": "Alice", "id": 100},
            {"name": "Bob", "id": 200},
            {"name": "Carol", "id": 300},
        ]
        result = filter_records(records, "id > 150")
        assert len(result) == 2
        assert result[0]["name"] == "Bob"

    def test_filter_with_in_operator(self):
        records = [
            {"name": "Alice", "status": "active"},
            {"name": "Bob", "status": "paused"},
            {"name": "Carol", "status": "incoming"},
        ]
        result = filter_records(records, 'status in ["active", "incoming"]')
        assert len(result) == 2

    def test_invalid_filter_skips_records(self):
        records = [
            {"name": "Alice", "status": "active"},
            {"name": "Bob"},
        ]
        result = filter_records(records, 'status == "active"')
        assert len(result) == 1
        assert result[0]["name"] == "Alice"


class TestBuildEmail:
    def test_basic_email(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test Subject",
            body="Test body",
        )
        assert msg["From"] == "sender@example.com"
        assert msg["To"] == "recipient@example.com"
        assert msg["Subject"] == "Test Subject"
        assert "Test body" in msg.as_string()

    def test_email_with_cc(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
            cc="cc@example.com",
        )
        assert msg["Cc"] == "cc@example.com"

    def test_email_without_cc(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
        )
        assert msg["Cc"] is None

    def test_email_has_date(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
        )
        assert msg["Date"] is not None

    def test_email_has_message_id(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
        )
        assert msg["Message-ID"] is not None
        assert "@" in msg["Message-ID"]

    def test_email_with_attachment(self):
        with tempfile.NamedTemporaryFile(delete=False, suffix=".txt") as f:
            f.write(b"attachment content")
            attachment_path = Path(f.name)

        try:
            msg = build_email(
                from_addr="sender@example.com",
                to="recipient@example.com",
                subject="Test",
                body="Body",
                attachments=[attachment_path],
            )
            assert msg.is_multipart()
        finally:
            attachment_path.unlink()

    def test_email_with_nonexistent_attachment_ignored(self):
        msg = build_email(
            from_addr="sender@example.com",
            to="recipient@example.com",
            subject="Test",
            body="Body",
            attachments=[Path("/nonexistent/file.txt")],
        )
        assert not msg.is_multipart()


class TestIntegration:
    def test_batch_workflow(self):
        records = [
            {"name": "Alice", "preferred_name": "Ali", "id": 1001, "status": "active"},
            {"name": "Bob", "preferred_name": "Bob", "id": 1002, "status": "paused"},
            {"name": "Carol", "preferred_name": "Carol", "id": 1003, "status": "active"},
        ]

        filtered = filter_records(records, 'status == "active"')
        assert len(filtered) == 2

        to_template = "u{{id}}@anu.edu.au"
        subject_template = "Hello {{preferred_name}}"
        body_template = "Dear {{name}},\n\nThis is a test."

        emails = []
        for record in filtered:
            to = substitute_template(to_template, record)
            subject = substitute_template(subject_template, record)
            body = substitute_template(body_template, record)
            emails.append((to, subject, body))

        assert emails[0] == (
            "u1001@anu.edu.au",
            "Hello Ali",
            "Dear Alice,\n\nThis is a test.",
        )
        assert emails[1] == (
            "u1003@anu.edu.au",
            "Hello Carol",
            "Dear Carol,\n\nThis is a test.",
        )

    def test_real_student_data_format(self):
        record = {
            "name": "Danny Bettay",
            "preferred_name": "Danny",
            "id": 6937496,
            "primary_supervisor": "Alex Zafiroglu",
            "supervisory_panel": ["Katherine Daniell", "Amy McLennan"],
            "commencement_date": "2020-07-27",
            "status": "active",
        }

        to = substitute_template("{{name}} <u{{id}}@anu.edu.au>", record)
        assert to == "Danny Bettay <u6937496@anu.edu.au>"

        body = substitute_template(
            "Hi {{preferred_name}},\n\nYour supervisor is {{primary_supervisor}}.",
            record,
        )
        assert body == "Hi Danny,\n\nYour supervisor is Alex Zafiroglu."


if __name__ == "__main__":
    import subprocess

    subprocess.run(["pytest", __file__, "-v"])
