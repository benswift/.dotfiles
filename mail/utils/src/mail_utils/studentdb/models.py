from pydantic import BaseModel, field_validator

# Ben's own person_id, used to derive `ben_role` on the denormalised output.
BEN_PERSON_ID = "ben_swift"

VALID_STATUSES = {
    "pre-confirmation",
    "confirmed",
    "submitted",
    "paused",
    "completed",
    "withdrawn",
}

# A student still in candidature. `submitted` counts --- the thesis is under
# examination but they're still enrolled --- as does `paused`.
ACTIVE_STATUSES = VALID_STATUSES - {"completed", "withdrawn"}


class Person(BaseModel):
    # `name` is the display full name ("Sui Jackson"); `legal_name` is what ANU's
    # systems hold ("Paul Jackson"), for looking people up in Dynamics/Wattle/HORUS.
    # Never put `legal_name` in prose --- that's what `name` is for.
    name: str
    legal_name: str | None = None
    preferred_name: str | None = None
    email: str | None = None
    alt_email: str | None = None

    def display_name(self) -> str:
        return self.preferred_name or self.name


class Student(BaseModel):
    person_id: str
    uid: str
    program: str = "PhD"
    primary_supervisor_id: str
    # Chair of Panel, who may or may not be the primary supervisor. `panel_ids`
    # holds the associate supervisors only --- panel minus primary minus chair.
    panel_chair_id: str | None = None
    panel_ids: list[str]
    status: str
    school: str | None = None
    commencement_date: str | None = None
    completion_date: str | None = None
    crp_chair_id: str | None = None
    thesis_title: str | None = None
    source: str | None = None
    notes: str | None = None

    def ben_role(self) -> str | None:
        """Ben's role on this candidature, derived rather than stored.

        Ordered most- to least-involved: he can be both primary and chair
        (Sui Jackson), and the strongest role is the one worth reporting.
        """
        if self.primary_supervisor_id == BEN_PERSON_ID:
            return "primary"
        if self.panel_chair_id == BEN_PERSON_ID:
            return "panel-chair"
        if BEN_PERSON_ID in self.panel_ids:
            return "associate"
        if self.crp_chair_id == BEN_PERSON_ID:
            return "crp-chair"
        return None

    @field_validator("status")
    @classmethod
    def validate_status(cls, v: str) -> str:
        if v not in VALID_STATUSES:
            raise ValueError(f"status must be one of {VALID_STATUSES}, got {v!r}")
        return v


class StudentDatabase(BaseModel):
    people: dict[str, Person]
    students: list[Student]

    def validate_references(self) -> list[str]:
        errors = []
        for student in self.students:
            if student.person_id not in self.people:
                errors.append(
                    f"student references unknown person_id: {student.person_id}"
                )
            if student.primary_supervisor_id not in self.people:
                errors.append(
                    f"student {student.person_id} references unknown supervisor: "
                    f"{student.primary_supervisor_id}"
                )
            if student.panel_chair_id and student.panel_chair_id not in self.people:
                errors.append(
                    f"student {student.person_id} references unknown panel_chair: "
                    f"{student.panel_chair_id}"
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
    legal_name: str | None = None
    preferred_name: str | None
    email: str | None
    alt_email: str | None = None
    uid: str
    program: str = "PhD"
    status: str
    school: str | None = None
    commencement_date: str | None
    completion_date: str | None = None
    ben_role: str | None = None
    supervisor: Person
    panel_chair: Person | None = None
    panel: list[Person]
    crp_chair: Person | None = None
    thesis_title: str | None = None
    source: str | None = None
    notes: str | None = None
