from pydantic import BaseModel, field_validator


class Person(BaseModel):
    name: str
    preferred_name: str | None = None
    email: str | None = None

    def display_name(self) -> str:
        return self.preferred_name or self.name


class Student(BaseModel):
    person_id: str
    uid: str
    primary_supervisor_id: str
    panel_ids: list[str]
    status: str
    commencement_date: str | None = None
    crp_chair_id: str | None = None

    @field_validator("status")
    @classmethod
    def validate_status(cls, v: str) -> str:
        valid = {"pre-confirmation", "confirmed", "paused", "completed"}
        if v not in valid:
            raise ValueError(f"status must be one of {valid}, got {v!r}")
        return v


class StudentDatabase(BaseModel):
    people: dict[str, Person]
    students: list[Student]

    def validate_references(self) -> list[str]:
        errors = []
        for student in self.students:
            if student.person_id not in self.people:
                errors.append(f"student references unknown person_id: {student.person_id}")
            if student.primary_supervisor_id not in self.people:
                errors.append(
                    f"student {student.person_id} references unknown supervisor: "
                    f"{student.primary_supervisor_id}"
                )
            for panel_id in student.panel_ids:
                if panel_id not in self.people:
                    errors.append(
                        f"student {student.person_id} references unknown panel member: {panel_id}"
                    )
            if student.crp_chair_id and student.crp_chair_id not in self.people:
                errors.append(
                    f"student {student.person_id} references unknown crp_chair: "
                    f"{student.crp_chair_id}"
                )
        return errors


class DenormalisedStudent(BaseModel):
    name: str
    preferred_name: str | None
    email: str | None
    uid: str
    status: str
    commencement_date: str | None
    supervisor: Person
    panel: list[Person]
    crp_chair: Person | None = None
