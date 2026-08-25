---
id: TASK-034
title: Evaluate a subscription-backed Grok Build runner for agent-run
status: To Do
assignee: []
created_date: '2026-08-24 01:17'
updated_date: '2026-08-25 03:40'
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
- [x] #1 The current Grok Build subscription and official tooling are tested to establish whether unattended CLI runs consume the subscription entitlement
- [x] #2 When subscription-backed headless execution is supported, `agent-run` exposes a named Grok Build profile that runs prompts in the requested working directory without requiring `OPENROUTER_API_KEY` or an xAI API-billing credential
- [x] #3 The runner preserves the common dispatcher contract where the Grok tooling supports it, and unsupported model, permission, or effort options fail clearly rather than being silently ignored
- [x] #4 Automated tests cover command construction, environment isolation, profile selection, and missing-authentication failures for the Grok route
- [ ] #5 Documentation distinguishes Grok subscription usage from OpenRouter and xAI API billing and includes an `agent-run` example plus a `bin/slopu` consumer example
- [ ] #6 If Grok does not expose subscription-backed unattended execution, the verified limitation and available API-billed alternatives are documented and no misleading profile is added
- [x] #7 The grok CLI is installed through mise (aqua:x.ai/cli/grok) via mise/config.toml rather than the curl|bash installer, so the pinned version matches on every machine and dotfiles doctor can verify it
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Verified on daysy 2026-08-25 against a fresh SuperGrok subscription, grok 1.0.5.

AC#1: subscription-backed headless execution IS supported. `grok -p` runs to
completion with only the grok.com OAuth credential in ~/.grok/auth.json and no
XAI_API_KEY or GROK_CODE_XAI_API_KEY anywhere in the environment. A run with a
deliberately bogus XAI_API_KEY exported still succeeded, confirming grok-sub's
unset list keeps it on the subscription route. The `total_cost_usd` field in
--output-format json is token accounting at API rates, not a charge.

Tier: plain SuperGrok or X Premium+ is enough; Heavy buys a larger allowance,
not access. The Heavy-only claim in circulation dates from the May 2026 beta.

AC#3 landed as a general fix: options are namespaced per runner and the
dispatcher rejects one aimed at a different runner than the profile selects.
It applies to claude and codex too, which previously dropped foreign options
silently. agent-run has no external callers yet, so nothing broke.

Grok reads Claude Code config natively --- CLAUDE.md at both levels, and
skills at user and project level. `grok inspect` in slop-university lists its
project `publish` skill, so a port needs no prompt rewrite.

AC#5 half-done: the billing distinction and an agent-run example are in
CLAUDE.md. `bin/slopu` does not exist in any repo, so its example is still
outstanding --- see the slop-university port note below.
AC#6 is moot: the capability exists, so no limitation to document and the
profile added is genuinely subscription-backed.

Remaining, for the slop-university handover on weddle: ops/cron-publish.sh
calls /home/ben/.local/bin/claude directly rather than going through agent-run,
and its auth-failure classifier greps for Claude's error strings ("please run
/login" etc.), which Grok does not emit. Both need porting before grok can be
put in charge of publishing.
<!-- SECTION:NOTES:END -->
