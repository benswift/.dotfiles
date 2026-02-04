import json
import subprocess
from pathlib import Path

from pydantic import ValidationError

from .models import DenormalisedStudent, Person, Student, StudentDatabase

__all__ = [
    "StudentDB",
    "StudentDatabase",
    "Student",
    "Person",
    "DenormalisedStudent",
    "load_from_nb",
    "load_from_file",
]

NB_NOTE_ID = "488"


def get_nb_path(note_id: str) -> Path:
    result = subprocess.run(
        ["nb", "show", note_id, "--path"],
        capture_output=True,
        text=True,
        check=True,
    )
    return Path(result.stdout.strip())


def load_from_file(path: Path) -> "StudentDB":
    try:
        data = json.loads(path.read_text())
    except json.JSONDecodeError as e:
        raise ValueError(f"invalid JSON in {path}: {e}") from e

    try:
        db = StudentDatabase.model_validate(data)
    except ValidationError as e:
        raise ValueError(f"schema validation failed for {path}: {e}") from e

    errors = db.validate_references()
    if errors:
        raise ValueError(f"reference validation failed: {'; '.join(errors)}")

    return StudentDB(db)


def load_from_nb(note_id: str = NB_NOTE_ID) -> "StudentDB":
    path = get_nb_path(note_id)
    return load_from_file(path)


class StudentDB:
    def __init__(self, db: StudentDatabase):
        self._db = db

    @classmethod
    def from_nb(cls, note_id: str = NB_NOTE_ID) -> "StudentDB":
        return load_from_nb(note_id)

    @classmethod
    def from_file(cls, path: Path) -> "StudentDB":
        return load_from_file(path)

    def _get_person(self, person_id: str) -> Person:
        return self._db.people[person_id]

    def _denormalise(self, student: Student) -> DenormalisedStudent:
        person = self._get_person(student.person_id)
        return DenormalisedStudent(
            name=person.name,
            preferred_name=person.preferred_name,
            email=person.email,
            uid=student.uid,
            status=student.status,
            commencement_date=student.commencement_date,
            supervisor=self._get_person(student.primary_supervisor_id),
            panel=[self._get_person(pid) for pid in student.panel_ids],
            crp_chair=(
                self._get_person(student.crp_chair_id) if student.crp_chair_id else None
            ),
        )

    def students(self, status: str | None = None) -> list[DenormalisedStudent]:
        results = []
        for student in self._db.students:
            if status and student.status != status:
                continue
            results.append(self._denormalise(student))
        return results

    def get_student(self, person_id: str) -> DenormalisedStudent | None:
        for student in self._db.students:
            if student.person_id == person_id:
                return self._denormalise(student)
        return None

    def people(self) -> dict[str, Person]:
        return self._db.people.copy()
