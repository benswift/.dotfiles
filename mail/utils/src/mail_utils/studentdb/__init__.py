import json
import subprocess
from pathlib import Path

from pydantic import ValidationError

from .models import (
    ACTIVE_STATUSES,
    DenormalisedStudent,
    Person,
    Student,
    StudentDatabase,
)

__all__ = [
    "ACTIVE_STATUSES",
    "StudentDB",
    "StudentDatabase",
    "Student",
    "Person",
    "DenormalisedStudent",
]

NB_NOTE_ID = "data/student-db.json"


class StudentDB:
    def __init__(self, db: StudentDatabase) -> None:
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
            legal_name=person.legal_name,
            preferred_name=person.preferred_name,
            email=person.email,
            alt_email=person.alt_email,
            uid=student.uid,
            program=student.program,
            status=student.status,
            school=student.school,
            commencement_date=student.commencement_date,
            completion_date=student.completion_date,
            ben_role=student.ben_role(),
            thesis_title=student.thesis_title,
            source=student.source,
            notes=student.notes,
            supervisor=self._get_person(student.primary_supervisor_id),
            panel_chair=(
                self._get_person(student.panel_chair_id)
                if student.panel_chair_id
                else None
            ),
            panel=[self._get_person(pid) for pid in student.panel_ids],
            crp_chair=(
                self._get_person(student.crp_chair_id) if student.crp_chair_id else None
            ),
        )

    def students(
        self,
        status: str | None = None,
        school: str | None = None,
        ben_role: str | None = None,
        active_only: bool = False,
    ) -> list[DenormalisedStudent]:
        """Filter students. All filters are independent and all default to off.

        The convenor-facing defaults (SOCY, active only) live in the CLI, not
        here --- callers of the library get everything unless they ask.
        """
        results = []
        for student in self._db.students:
            if status and student.status != status:
                continue
            if active_only and student.status not in ACTIVE_STATUSES:
                continue
            if school and student.school != school:
                continue
            if ben_role:
                role = student.ben_role()
                if role is None or (ben_role != "any" and role != ben_role):
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
