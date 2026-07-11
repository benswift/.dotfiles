---
id: TASK-021
title: >-
  Build TypeScript CLI to post to LinkedIn (personal profile + LLMs Unplugged
  org page)
status: To Do
assignee: []
created_date: '2026-07-11 22:34'
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

A small TypeScript CLI that publishes text posts to LinkedIn, targeting either Ben's personal profile or the (not-yet-created) "LLMs Unplugged" organization Page.

## Library

Official `linkedin-api-client` (npm; TypeScript, Axios-based, Node-only --- LinkedIn rejects browser calls on CORS). Provides a generic `RestliClient` plus an `AuthClient` for generating/refreshing OAuth tokens. Beta but LinkedIn-maintained; still the best fit vs hand-rolling the Rest.li request encoding. (The similarly-named `linkedin-private-api` is an unofficial credential-based scraper --- ToS violation, do not use.)

## API

Posts API, `POST /rest/posts` via `RestliClient.create()`. The author URN switches the target:

- personal: `urn:li:person:{personId}` --- needs the `w_member_social` scope (from the "Share on LinkedIn" product, self-serve).
- org page: `urn:li:organization:{orgId}` --- needs the `w_organization_social` scope (from the "Community Management API" product, which requires an app-review application, and Ben must hold an admin role on the Page).

## Auth

OAuth 2.0 authorization-code flow. One-time browser consent mints an access token (60-day life) + refresh token (365-day life); `AuthClient` refreshes silently thereafter. A free LinkedIn account is sufficient --- no Premium needed.

## Secrets

client_id/client_secret and the refresh tokens live behind fnox (`op://` refs). Access tokens are cached locally (e.g. ~/.cache/linkedin-post) and refreshed on demand. Nothing secret in a tracked file.

## Open decisions

- Project location: doesn't fit dotfiles `bin/` (needs node_modules), so likely a standalone pnpm package/repo --- confirm.
- CLI surface: e.g. `linkedin-post personal "text"` / `linkedin-post llms-unplugged "text"`, with author URNs resolved from config.
- v1 scope: text-only; images/carousels/articles deferred.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 CLI posts a text update to Ben's personal profile
- [ ] #2 CLI posts a text update to the LLMs Unplugged org Page
- [ ] #3 Tokens refresh automatically without re-consent until the 365-day refresh token lapses
- [ ] #4 client secret and refresh tokens are stored via fnox/1Password; no secrets in a tracked file
- [ ] #5 One-time browser setup steps are documented in the project README
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
1. (browser, BLOCKED) Create the "LLMs Unplugged" LinkedIn Page from Ben's account.
2. (browser, BLOCKED) Create a LinkedIn developer app; associate it with a Page (can be LLMs Unplugged).
3. (browser, BLOCKED) Enable products: "Sign In with LinkedIn using OpenID Connect" + "Share on LinkedIn"; apply for "Community Management API" for org posting (has review lead time).
4. Scaffold a pnpm TS project; add `linkedin-api-client`.
5. Implement one-shot OAuth: localhost redirect catcher -> exchange code -> store refresh token in fnox, cache access token.
6. Implement AuthClient refresh-on-expiry.
7. Implement the post command using RestliClient.create against /rest/posts, author URN chosen by target.
8. Wire config for personId + orgId + author-target aliases.
9. Docs: README covering the one-time browser setup + usage.
<!-- SECTION:PLAN:END -->
