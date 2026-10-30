---
name: task
folder: tasks
match:
  - tasks/*.md
filename: dated
schema:
  type: object
  properties:
    title: { type: string, minLength: 1 }
    tags: { type: array, items: { type: string } }
    status: { enum: [open, blocked, done] }
    due_date: { $ref: common.json#/$defs/date }
    agent: { $ref: common.json#/$defs/link }
  additionalProperties: false
---

Something someone has to do, dated by when it was made.
