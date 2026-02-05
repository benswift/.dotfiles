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
]

NB_NOTE_ID = "488"


class StudentDB:
    def __init__(self, db: StudentDatabase):
        self._db = db

    @classmethod
    def from_file(cls, path: Path) -> "StudentDB":
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

        return cls(db)

    @classmethod
    def from_nb(cls, note_id: str = NB_NOTE_ID) -> "StudentDB":
        result = subprocess.run(
            ["nb", "show", note_id, "--path"],
            capture_output=True,
            text=True,
            check=True,
        )
        path = Path(result.stdout.strip())
        return cls.from_file(path)

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
