"""Tests for the student database module."""

import json
from pathlib import Path

import pytest
from pydantic import ValidationError

from mail_utils.studentdb import (
    DenormalisedStudent,
    Person,
    Student,
    StudentDB,
    StudentDatabase,
    load_from_file,
)


VALID_DATABASE = {
    "people": {
        "alice_student": {
            "name": "Alice Student",
            "preferred_name": "Ali",
            "email": "alice@example.com",
        },
        "bob_student": {
            "name": "Bob Student",
            "email": "bob@example.com",
        },
        "carol_supervisor": {
            "name": "Carol Supervisor",
            "preferred_name": "Carol",
            "email": "carol@example.com",
        },
        "dan_panel": {
            "name": "Dan Panel",
            "email": "dan@example.com",
        },
        "eve_chair": {
            "name": "Eve Chair",
            "email": "eve@example.com",
        },
    },
    "students": [
        {
            "person_id": "alice_student",
            "uid": "u1234567",
            "primary_supervisor_id": "carol_supervisor",
            "panel_ids": ["dan_panel"],
            "status": "confirmed",
            "commencement_date": "2024-01-15",
            "crp_chair_id": "eve_chair",
        },
        {
            "person_id": "bob_student",
            "uid": "u7654321",
            "primary_supervisor_id": "carol_supervisor",
            "panel_ids": ["dan_panel", "eve_chair"],
            "status": "pre-confirmation",
        },
    ],
}


class TestPerson:
    def test_creates_person_with_all_fields(self):
        person = Person(
            name="Test Person", preferred_name="Test", email="test@example.com"
        )
        assert person.name == "Test Person"
        assert person.preferred_name == "Test"
        assert person.email == "test@example.com"

    def test_creates_person_with_required_fields_only(self):
        person = Person(name="Test Person")
        assert person.name == "Test Person"
        assert person.preferred_name is None
        assert person.email is None

    def test_display_name_returns_preferred_name_when_set(self):
        person = Person(name="Full Name", preferred_name="Nick")
        assert person.display_name() == "Nick"

    def test_display_name_returns_name_when_no_preferred(self):
        person = Person(name="Full Name")
        assert person.display_name() == "Full Name"


class TestStudent:
    def test_creates_student_with_all_fields(self):
        student = Student(
            person_id="test",
            uid="u1234567",
            primary_supervisor_id="supervisor",
            panel_ids=["panel1", "panel2"],
            status="confirmed",
            commencement_date="2024-01-01",
            crp_chair_id="chair",
        )
        assert student.person_id == "test"
        assert student.uid == "u1234567"
        assert student.status == "confirmed"
        assert student.crp_chair_id == "chair"

    def test_creates_student_without_optional_fields(self):
        student = Student(
            person_id="test",
            uid="u1234567",
            primary_supervisor_id="supervisor",
            panel_ids=[],
            status="pre-confirmation",
        )
        assert student.commencement_date is None
        assert student.crp_chair_id is None

    def test_rejects_invalid_status(self):
        with pytest.raises(ValidationError) as exc_info:
            Student(
                person_id="test",
                uid="u1234567",
                primary_supervisor_id="supervisor",
                panel_ids=[],
                status="invalid",
            )
        assert "status must be one of" in str(exc_info.value)


class TestStudentDatabase:
    def test_validates_valid_database(self):
        db = StudentDatabase.model_validate(VALID_DATABASE)
        assert len(db.people) == 5
        assert len(db.students) == 2

    def test_validate_references_passes_for_valid_data(self):
        db = StudentDatabase.model_validate(VALID_DATABASE)
        errors = db.validate_references()
        assert errors == []

    def test_validate_references_detects_unknown_person_id(self):
        data = {
            "people": {"supervisor": {"name": "Supervisor"}},
            "students": [
                {
                    "person_id": "unknown",
                    "uid": "u1",
                    "primary_supervisor_id": "supervisor",
                    "panel_ids": [],
                    "status": "confirmed",
                }
            ],
        }
        db = StudentDatabase.model_validate(data)
        errors = db.validate_references()
        assert any("unknown person_id" in e for e in errors)

    def test_validate_references_detects_unknown_supervisor(self):
        data = {
            "people": {"student": {"name": "Student"}},
            "students": [
                {
                    "person_id": "student",
                    "uid": "u1",
                    "primary_supervisor_id": "unknown",
                    "panel_ids": [],
                    "status": "confirmed",
                }
            ],
        }
        db = StudentDatabase.model_validate(data)
        errors = db.validate_references()
        assert any("unknown supervisor" in e for e in errors)

    def test_validate_references_detects_unknown_panel_member(self):
        data = {
            "people": {
                "student": {"name": "Student"},
                "supervisor": {"name": "Supervisor"},
            },
            "students": [
                {
                    "person_id": "student",
                    "uid": "u1",
                    "primary_supervisor_id": "supervisor",
                    "panel_ids": ["unknown"],
                    "status": "confirmed",
                }
            ],
        }
        db = StudentDatabase.model_validate(data)
        errors = db.validate_references()
        assert any("unknown panel member" in e for e in errors)


class TestLoadFromFile:
    def test_loads_valid_file(self, tmp_path: Path):
        db_file = tmp_path / "db.json"
        db_file.write_text(json.dumps(VALID_DATABASE))

        db = load_from_file(db_file)
        assert len(db.students()) == 2

    def test_raises_on_invalid_json(self, tmp_path: Path):
        db_file = tmp_path / "db.json"
        db_file.write_text("not valid json")

        with pytest.raises(ValueError) as exc_info:
            load_from_file(db_file)
        assert "invalid JSON" in str(exc_info.value)

    def test_raises_on_schema_validation_error(self, tmp_path: Path):
        db_file = tmp_path / "db.json"
        db_file.write_text(json.dumps({"people": {}, "students": [{"bad": "data"}]}))

        with pytest.raises(ValueError) as exc_info:
            load_from_file(db_file)
        assert "schema validation failed" in str(exc_info.value)

    def test_raises_on_reference_validation_error(self, tmp_path: Path):
        data = {
            "people": {"supervisor": {"name": "Supervisor"}},
            "students": [
                {
                    "person_id": "unknown",
                    "uid": "u1",
                    "primary_supervisor_id": "supervisor",
                    "panel_ids": [],
                    "status": "confirmed",
                }
            ],
        }
        db_file = tmp_path / "db.json"
        db_file.write_text(json.dumps(data))

        with pytest.raises(ValueError) as exc_info:
            load_from_file(db_file)
        assert "reference validation failed" in str(exc_info.value)


class TestStudentDB:
    @pytest.fixture
    def db(self, tmp_path: Path) -> StudentDB:
        db_file = tmp_path / "db.json"
        db_file.write_text(json.dumps(VALID_DATABASE))
        return StudentDB.from_file(db_file)

    def test_students_returns_all_students(self, db: StudentDB):
        students = db.students()
        assert len(students) == 2

    def test_students_filters_by_status(self, db: StudentDB):
        confirmed = db.students(status="confirmed")
        assert len(confirmed) == 1
        assert confirmed[0].name == "Alice Student"

        preconf = db.students(status="pre-confirmation")
        assert len(preconf) == 1
        assert preconf[0].name == "Bob Student"

    def test_students_returns_empty_for_unknown_status(self, db: StudentDB):
        students = db.students(status="completed")
        assert students == []

    def test_students_returns_denormalised_data(self, db: StudentDB):
        students = db.students(status="confirmed")
        alice = students[0]

        assert isinstance(alice, DenormalisedStudent)
        assert alice.name == "Alice Student"
        assert alice.preferred_name == "Ali"
        assert alice.email == "alice@example.com"
        assert alice.uid == "u1234567"
        assert alice.status == "confirmed"
        assert alice.commencement_date == "2024-01-15"

        assert alice.supervisor.name == "Carol Supervisor"
        assert alice.supervisor.email == "carol@example.com"

        assert len(alice.panel) == 1
        assert alice.panel[0].name == "Dan Panel"

        assert alice.crp_chair is not None
        assert alice.crp_chair.name == "Eve Chair"

    def test_students_handles_missing_optional_fields(self, db: StudentDB):
        students = db.students(status="pre-confirmation")
        bob = students[0]

        assert bob.preferred_name is None
        assert bob.commencement_date is None
        assert bob.crp_chair is None

    def test_get_student_returns_student_by_id(self, db: StudentDB):
        alice = db.get_student("alice_student")
        assert alice is not None
        assert alice.name == "Alice Student"

    def test_get_student_returns_none_for_unknown_id(self, db: StudentDB):
        result = db.get_student("unknown")
        assert result is None

    def test_people_returns_all_people(self, db: StudentDB):
        people = db.people()
        assert len(people) == 5
        assert "carol_supervisor" in people
        assert people["carol_supervisor"].name == "Carol Supervisor"
