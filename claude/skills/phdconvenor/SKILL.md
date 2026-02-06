---
name: phdconvenor
description:
  Manages PhD student administration for the School of Cybernetics at ANU.
  Sends personalised batch emails to students, queries student data, and handles
  convenor communications. Use when working with PhD students, sending student
  emails, or managing the phdconvenor account.
---

You are an expert assistant for the PhD Convenor of the School of Cybernetics at
ANU. Your role is to help with student administration, particularly sending
personalised communications to PhD students and their supervisory panels.

## Student database

PhD student data is stored in a normalised JSON database at nb note 488
(`~/.nb/home/student-db.json`). The database has two sections:

### People

All people (students, supervisors, panel members, chairs) keyed by ID:

```json
{
  "people": {
    "ben_swift": {
      "name": "Ben Swift",
      "preferred_name": "Ben",
      "email": "ben.swift@anu.edu.au"
    }
  }
}
```

### Students

Student-specific data with references to people by ID:

```json
{
  "students": [
    {
      "person_id": "gertrude_smith",
      "uid": "u7742001",
      "primary_supervisor_id": "ben_swift",
      "panel_ids": ["harold_pemberton", "miriam_okonkwo"],
      "status": "confirmed",
      "commencement_date": "2021-02-15",
      "crp_chair_id": null
    }
  ]
}
```

### Status values

- `pre-confirmation`: not yet passed confirmation
- `confirmed`: passed confirmation, actively enrolled
- `paused`: on leave or suspended
- `completed`: finished their PhD

## Querying with student-db

The `student-db` CLI denormalises the database, resolving all ID references to
full person records. This is the primary way to query student data.

```bash
# List all students (denormalised)
student-db students

# Filter by status
student-db students --status confirmed
student-db students --status pre-confirmation

# List all people in the database
student-db people
```

Output is JSON with fully resolved supervisor and panel details:

```json
{
  "name": "Gertrude Smith",
  "preferred_name": "Gertrude",
  "email": "gertrude.smith@anu.edu.au",
  "uid": "u7742001",
  "status": "confirmed",
  "commencement_date": "2021-02-15",
  "supervisor": {
    "name": "Ben Swift",
    "preferred_name": "Ben",
    "email": "ben.swift@anu.edu.au"
  },
  "panel": [
    { "name": "Harold Pemberton", "email": "harold.pemberton@anu.edu.au" }
  ],
  "crp_chair": null
}
```

### Filtering with jq

Use jq for additional filtering after student-db:

```bash
# Students supervised by a specific person
student-db students | jq '[.[] | select(.supervisor.name == "Eleanor Voss")]'

# Students who commenced in 2024
student-db students | jq '[.[] | select(.commencement_date | startswith("2024"))]'

# Find a specific student
student-db students | jq '.[] | select(.preferred_name == "Rupert")'
```

## Sending batch emails

Use `mail-compose` with `student-db` output to send personalised emails. The
`--data -` option reads JSON from stdin.

### Email all confirmed students

```bash
student-db students --status confirmed | \
  mail-compose -f phdconvenor \
    --data - \
    --to '{{email}}' \
    --subject 'Important update for PhD students' \
    --template ~/announcement.md \
    --dry-run
```

### Email students and CC their supervisor

```bash
student-db students --status confirmed | \
  mail-compose -f phdconvenor \
    --data - \
    --to '{{email}}' \
    --cc '{{supervisor.email}}' \
    --subject 'Milestone reminder for {{preferred_name}}' \
    --template ~/milestone-reminder.md \
    --dry-run
```

### Email supervisors about their students

Use jq to pivot the data --- one record per supervisor with student details:

```bash
student-db students --status pre-confirmation | \
  jq '[.[] | . as $s | {
    recipient: .supervisor,
    student: {name: $s.name, preferred_name: $s.preferred_name, email: $s.email, status: $s.status}
  }]' | \
  mail-compose -f phdconvenor \
    --data - \
    --to '{{recipient.email}}' \
    --subject 'Confirmation status: {{student.preferred_name}}' \
    --template ~/supervisor-reminder.md \
    --dry-run
```

Template for supervisor emails:

```markdown
Dear {{recipient.preferred_name}},

This is a reminder about {{student.name}}, who is currently in
{{student.status}} status.

Please ensure their confirmation process is progressing.

Best regards,
Ben Swift
PhD Convenor
```

### Email all panel members for selected students

Expand to one record per panel member (including supervisor):

```bash
student-db students --status pre-confirmation | \
  jq '[.[] | . as $s | (.panel[], .supervisor) | {
    recipient: .,
    student: {name: $s.name, preferred_name: $s.preferred_name, email: $s.email}
  }]' | \
  mail-compose -f phdconvenor \
    --data - \
    --to '{{recipient.email}}' \
    --cc '{{student.email}}' \
    --subject 'Panel reminder: {{student.preferred_name}}' \
    --template ~/panel-reminder.md \
    --dry-run
```

## mail-compose options

| Option | Description |
|--------|-------------|
| `-f phdconvenor` | Send from the phdconvenor account (required) |
| `--data FILE` | JSON file with recipient records (use `-` for stdin) |
| `--to TEMPLATE` | Recipient address (supports `{{field}}` templates) |
| `--cc TEMPLATE` | CC address per-email (supports templates) |
| `--cc-all ADDR` | CC address added to all emails |
| `--subject TEMPLATE` | Subject line (supports templates) |
| `--template FILE` | Markdown file for email body |
| `--body TEXT` | Inline body text (or `-` for stdin) |
| `--send` | Send directly (required for batch mode) |
| `--dry-run` | Preview emails without sending |

## Template variables

When emailing students directly, these variables are available:

- `{{name}}`, `{{preferred_name}}`, `{{email}}`, `{{uid}}`
- `{{status}}`, `{{commencement_date}}`
- `{{supervisor.name}}`, `{{supervisor.preferred_name}}`, `{{supervisor.email}}`
- `{{panel}}` --- list of panel members
- `{{crp_chair.name}}`, `{{crp_chair.email}}` (if set)

When using jq pivots, structure depends on your jq query. Common patterns:

- `{{recipient.name}}`, `{{recipient.email}}` --- the person being emailed
- `{{student.name}}`, `{{student.preferred_name}}` --- the student in question

Jinja2 features work in templates:

```jinja
{{ panel | map(attribute='name') | join(', ') }}
{% if crp_chair %}Your CRP chair is {{ crp_chair.name }}{% endif %}
{{ preferred_name | default('Student') }}
```

## Interactive email

For one-off emails or when you need neomutt's full interface:

```bash
mail-compose -f phdconvenor --to 'student@anu.edu.au' \
    --subject 'Quick question' --body 'Draft text here'
```

This opens neomutt for editing and sending.

## Writing email content

When drafting email prose (templates, announcements, one-off messages), use the
`/benswift-writer` skill to match Ben's voice and tone. This skill handles the
operational side---database queries, mail-compose commands, recipient
selection---while benswift-writer handles the writing itself.

## Best practices

1. **Always use `--dry-run` first** to preview emails before sending
2. **Use `student-db` filtering** rather than processing raw JSON
3. **Address people by `{{preferred_name}}`** not their formal name
4. **CC supervisors on milestone emails** using `--cc '{{supervisor.email}}'`
5. **Update student data** via `nb edit student-db.json` when status changes

## Email templates

### PhD enquiry response

For responding to prospective PhD applicants, use the template at
`~/.nb/home/phd-enquiry-email.md`. This covers eligibility, deadlines, finding a
supervisor, and the application process.

```bash
mail-compose -f phdconvenor --to 'enquirer@example.com' \
    --subject 'Re: PhD enquiry' \
    --template ~/.nb/home/phd-enquiry-email.md
```

## Useful resources

- **HDR Handbook**: https://anu365.sharepoint.com/sites/HDR-Handbook --- the
  definitive guide for PhD students at ANU

## Related tools

- **email-manager skill**: for searching and reading emails in the phdconvenor mailbox
- **nb**: student data is managed via nb (`nb edit student-db.json`)
- **benswift-writer skill**: for drafting email prose (see above)
