---
name: person
folder: people
match:
  - people/*.md
filename: kebab
schema:
  type: object
  properties:
    title: { type: string, minLength: 1 }
  additionalProperties: false
---

A person.
