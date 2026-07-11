---
id: TASK-021
title: >-
  Build uv single-file Python CLI to post to LinkedIn (personal profile + LLMs
  Unplugged org page)
status: To Do
assignee: []
created_date: '2026-07-11 22:34'
updated_date: '2026-07-11 22:56'
labels:
  - linkedin
  - tooling
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Design captured while away from a browser; the OAuth consent steps in the plan are blocked until Ben's at a machine with a browser.

## Goal

A single-file CLI that publishes text posts to LinkedIn, targeting either Ben's personal profile or the (not-yet-created) "LLMs Unplugged" organization Page.

## Shape

A uv single-file Python script at `bin/linkedin-post`, in the same house style as `bin/ingest-claude-logs`: `#!/usr/bin/env -S uv run --script` shebang, PEP 723 inline metadata for the one dependency (`httpx`), Python 3.12+ with type hints that pass `ty`. No project scaffolding, no `package.json`/`node_modules` --- it drops into `bin/` alongside the existing single-file scripts.

Chosen over a TypeScript/bun equivalent for fit: more of `bin/` is already uv-Python, uv is central to the stack (bun is globals-only), PEP 723 pins the dependency, and `ty` type-checking is already in the loop. The only thing given up vs bun is bun's zero-dep native `fetch`/`Bun.serve`; one pinned `httpx` dep is a fine trade (stdlib `urllib` + `http.server` would keep it dep-free but uglier).

## No library

Deliberately no `linkedin-api-client`. That library's value is untangling Rest.li encoding for finder/GET queries with URNs in query params; posting is a single `POST /rest/posts` with a JSON body, and the OAuth token exchange/refresh are plain POSTs --- all just `httpx`. Hand-rolling avoids a beta dependency and its churn. (The unofficial credential-scraper `linkedin-private-api` is a ToS violation --- do not use.)

## API

Posts API, `POST https://api.linkedin.com/rest/posts`, headers `LinkedIn-Version: YYYYMM`, `X-Restli-Protocol-Version: 2.0.0`, `Authorization: Bearer`. The author URN switches the target:

- personal: `urn:li:person:{personId}` --- needs the `w_member_social` scope (from the "Share on LinkedIn" product, self-serve).
- org page: `urn:li:organization:{orgId}` --- needs the `w_organization_social` scope (from the "Community Management API" product, which requires an app-review application with real lead time, and Ben must hold an admin role on the Page).

## Auth

OAuth 2.0 authorization-code flow. A one-time browser consent (stdlib `http.server` catches the localhost redirect, ~10 lines) mints an access token (60-day life) + refresh token (365-day life); refresh silently via `httpx` thereafter. A free LinkedIn account is sufficient --- no Premium.

## Secrets

client_id/client_secret and the refresh tokens live behind fnox (`op://` refs), read by shelling out to `fnox get`. The access token is cached locally (e.g. under $XDG_STATE_HOME) and refreshed on demand. Nothing secret in a tracked file.

## CLI surface

`linkedin-post personal "text"` / `linkedin-post llms-unplugged "text"`; author URNs resolved from a small in-script config map. v1 is text-only; images/carousels/articles deferred.

## Provisioning

Bun is not needed. uv is already available everywhere; no new tool to add. (If it were TS, bun would need pinning in mise/config.toml --- avoided here.)
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 CLI posts a text update to Ben's personal profile
- [ ] #2 CLI posts a text update to the LLMs Unplugged org Page
- [ ] #3 Tokens refresh automatically without re-consent until the 365-day refresh token lapses
- [ ] #4 client secret and refresh tokens are stored via fnox/1Password; no secrets in a tracked file
- [ ] #5 One-time browser setup is documented in the script's header comment (and dotfiles CLAUDE.md if warranted)
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
1. (browser, BLOCKED) Create the "LLMs Unplugged" LinkedIn Page from Ben's account.
2. (browser, BLOCKED) Create a LinkedIn developer app; associate it with a Page (can be LLMs Unplugged).
3. (browser, BLOCKED) Enable products: "Sign In with LinkedIn using OpenID Connect" + "Share on LinkedIn"; apply for "Community Management API" for org posting (has review lead time --- kick off first).
4. Write bin/linkedin-post: uv single-file shebang + PEP 723 header (httpx), typed, ty-clean.
5. Implement one-shot OAuth: stdlib http.server localhost catcher -> exchange code -> store refresh token in fnox, cache access token under XDG state.
6. Implement refresh-on-expiry against the LinkedIn token endpoint.
7. Implement the post command: POST /rest/posts, author URN chosen by the target arg.
8. Wire the config map for personId + orgId + target aliases.
9. Document the one-time browser setup in the script's header comment; add a bin/ entry to dotfiles CLAUDE.md if warranted.
10. Add a tests/ entry if any pure logic (e.g. token-cache staleness) is worth covering.
<!-- SECTION:PLAN:END -->
