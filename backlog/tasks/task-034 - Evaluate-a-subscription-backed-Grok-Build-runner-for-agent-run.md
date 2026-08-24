---
id: TASK-034
title: Evaluate a subscription-backed Grok Build runner for agent-run
status: To Do
assignee: []
created_date: '2026-08-24 01:17'
labels: []
dependencies: []
references:
  - bin/agent-run
  - agent-run/profiles.toml
  - 'https://openrouter.ai/docs/guides/overview/auth/byok'
priority: medium
type: enhancement
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The shared `agent-run` dispatcher can select Grok Build through OpenRouter, but that route consumes API credits. Once a Grok Build subscription is available for testing, determine whether its official tooling exposes a headless coding-agent route that consumes the subscription entitlement. If it does, add a native runner/profile so projects such as slop-university can use the subscription without OpenRouter or xAI API billing. Do not present an API-key route as subscription-backed.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 The current Grok Build subscription and official tooling are tested to establish whether unattended CLI runs consume the subscription entitlement
- [ ] #2 When subscription-backed headless execution is supported, `agent-run` exposes a named Grok Build profile that runs prompts in the requested working directory without requiring `OPENROUTER_API_KEY` or an xAI API-billing credential
- [ ] #3 The runner preserves the common dispatcher contract where the Grok tooling supports it, and unsupported model, permission, or effort options fail clearly rather than being silently ignored
- [ ] #4 Automated tests cover command construction, environment isolation, profile selection, and missing-authentication failures for the Grok route
- [ ] #5 Documentation distinguishes Grok subscription usage from OpenRouter and xAI API billing and includes an `agent-run` example plus a `bin/slopu` consumer example
- [ ] #6 If Grok does not expose subscription-backed unattended execution, the verified limitation and available API-billed alternatives are documented and no misleading profile is added
<!-- AC:END -->
