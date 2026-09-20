---
id: TASK-035
title: Restructure benswift-writer around per-context before/after pairs
status: To Do
assignee: []
created_date: '2026-08-29 01:14'
updated_date: '2026-09-20 22:10'
labels: []
dependencies: []
references:
  - ~/.claude/plugins/marketplaces/ben/skills/benswift-writer/SKILL.md
  - ~/.claude/plugins/marketplaces/ben/drafts/benswift-writer/
  - ~/projects/benswift-me/src/content/blog/
  - ~/projects/research-papers/
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The benswift-writer skill describes Ben's voice as a list of rules that apply to every register at once, so the same overlay is used for a colleague email, a student DM, a blog post and a paper, and the model has to guess how the rules bend in each. Replace the rules-for-everything approach with contrastive examples per writing context: short pairs of an AI-drafted paragraph (before) and the version in Ben's voice (after), each with a one-line gloss naming the move. Pairs show the delta the model must apply, which a rules list or a pile of finished writing does not. Each context lives in its own file under contexts/, read on demand, so one register's examples never pollute another's context. The trope-detection pass and ai-tropes.md stay as they are.

Sources for the 'after' half: real blog paragraphs (benswift-me), real sent email (anonymised) and real paper paragraphs (research-papers), plus hand-edits of AI drafts. At least one pair per context must use Ben's from-scratch writing, because an edit of an AI draft keeps the draft's skeleton and misses the structural differences (where a paragraph starts, what it leaves unsaid, how it stops). The 'before' for a real-writing pair is generated blind from a content brief, never by showing the model the real version.

Candidate material (real paragraphs with content briefs) is already collected in the plugin repo under drafts/benswift-writer/; the remaining work is generating blind befores, Ben editing/approving pairs, and restructuring SKILL.md.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 SKILL.md keeps the universal quirks and the trope pass, and instructs the model to identify the writing context and read exactly one contexts/<name>.md, with a stated nearest-match fallback for contexts not listed
- [ ] #2 contexts/ contains blog.md, email-colleague.md, email-student.md and academic.md, each opening with a register brief (audience, typical length, formality, sign-off, jamesian preset, what is out of bounds) followed by 3-5 before/after pairs with a one-line gloss per pair and the 'after' placed second
- [ ] #3 In every context file at least one pair's 'after' is Ben's real from-scratch writing, and every 'before' was written blind from a content brief without sight of the 'after'
- [ ] #4 Pairs within a file cover distinct topics (no two on the same subject) and each context file is under ~1500 words
- [ ] #5 The jamesian preset choice (BALANCED/LAYERED) lives in each context brief, not in SKILL.md
- [ ] #6 The SKILL.md description is a folded scalar naming the contexts so the skill triggers on 'reply to this student' or 'draft an email to' as well as 'blog post'
- [ ] #7 ai-tropes.md and the detection-pass instructions are unchanged in substance
- [ ] #8 Holdout check done: for each context one brief was held back from the file, the skill's output on that brief compared blind against Ben's real version, and the result recorded in the task notes
- [ ] #9 Email excerpts in the skill are anonymised (no names, addresses, student IDs) and no candidate material is committed to the public dotfiles repo
- [ ] #10 Changes committed and pushed in claude-plugin-personal, and the benswift-writer entry in ~/.claude/CLAUDE.md's writing-rules paragraph still describes the skill chain accurately
- [ ] #11 SKILL.md's workflow ends with a capture step that fires only when Ben rewrites the model's output (not as a standing offer): it anonymises, appends the pair --- model draft, Ben's version, the context it was drafting in --- to inbox.md, commits and pushes in the plugin repo, and prints the running inbox count
- [ ] #12 inbox.md sits beside SKILL.md, outside contexts/, and is never read while drafting; its own header documents the promote rule --- on 'promote the writer inbox' entries move into the matching contexts/ file under a hard cap of five pairs, so adding one retires the weakest
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
Work in the marketplace clone (~/.claude/plugins/marketplaces/ben), never a second checkout, and push after every commit: `dotfiles update` re-clones it and discards local-only commits.

pairs.md (step 3) is single-use scaffolding: one file for Ben to make his whole pass in, thrown away at step 7. The split at step 4 is one-way --- from there on `skills/benswift-writer/contexts/` is canonical, and a later gloss fix, a swapped pair or a new context is edited there directly. Never reassemble a working file from the shipped ones.

1. Candidates are in drafts/benswift-writer/candidates-{blog,email,academic}.md. Each candidate has a neutral content `brief` and Ben's real text. Read the `## Notes` at the end of each file for what was rejected and why; candidates flagged AI-assisted are only usable as befores, not afters.

2. Generate the blind befores, **with opus**. For each candidate, spawn a fresh opus subagent with NO writing skill loaded, given only: the audience (from the context), the brief, and a target length. It must not see Ben's text. Opus rather than the usual sonnet default because the before has to be the honest default of the model Ben actually writes with --- a sonnet before would teach the overlay to correct a draft he never receives. Do not have one agent write several befores in a row; each one fresh, so they don't converge.

3. Assemble drafts/benswift-writer/pairs.md: one throwaway working file, all four contexts, already in the shape the shipped files take. Per context: a draft register brief (audience, length, formality, sign-off, jamesian preset, out of bounds), then every candidate as before-then-after with an empty gloss line. Ben's pass happens here and nowhere else --- keep or hand-edit each after, write the gloss naming the move (what the before did that his version doesn't: 'dropped the pre-emptive apology; the ask goes first', 'no summary sentence at the end', 'one concrete number instead of three adjectives'), correct the register briefs, keep 3-5 pairs per context on distinct topics, and mark the cuts and one HOLDOUT per context inline rather than deleting them.

4. Split pairs.md into skills/benswift-writer/contexts/{blog,email-colleague,email-student,academic}.md: drop the cut pairs and the holdouts, carry the briefs and surviving pairs across verbatim, and check each file lands under ~1500 words. Diff the pair count per context against pairs.md before moving on --- the split is the one place a pair can go missing. Op-ed is deferred: no real corpus yet.

5. Rewrite SKILL.md into a router: keep Language/Structure/Voice quirks and the whole AI-trope pass; move 'Email: Cheers' and the jamesian preset choice into the context briefs; add the step 'identify the context, read exactly one contexts/ file; for an unlisted context pick the nearest and say which'. Description as a folded scalar listing the contexts and trigger phrases.

6. Add the capture step as the last workflow item in SKILL.md, and ship inbox.md beside it (empty but for the header). Capture is event-triggered --- Ben rewrote the output --- never a standing offer, because a step that asks after every draft is one he learns to skip. It anonymises before it writes (most captures come from real student and colleague mail) and pushes in the same breath, or the next `dotfiles update` re-clone eats the entry. The gloss is left blank: naming the move is curation-time work and is easier across three failures than one. inbox.md's header carries the promote rule, so the file explains itself whenever it is next opened.

7. Holdout check: with the new skill loaded, generate each context's holdout brief and read it next to Ben's real version. Record pass/fail per context in the notes. A fail means the pairs teach the wrong move; adjust glosses or swap a pair, don't add more pairs.

8. Delete drafts/benswift-writer/ (pairs.md and the candidate files both), commit in the plugin repo (scope: benswift-writer), push, then `claude plugin marketplace update` so the served cache matches. Update the writing-rules paragraph in ~/.claude/CLAUDE.md only if the chain changed.

Later, once the pairs are in use: mine real failures from session logs (~/claude-logs, AI drafts Ben actually rewrote) as a second source of befores, and add an op-ed context when there is a corpus. Both land as direct edits to contexts/, under the five-pair cap.
<!-- SECTION:PLAN:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Candidate collection done 2026-08-29 and pushed to claude-plugin-personal (7fa328b) under drafts/benswift-writer/. Ben has not yet reviewed or edited anything; every remaining step needs a keyboard.

Blog (candidates-blog.md, B1-B8): 6 from 2025-2026 and 2 older (2019, 2021), all with plain human commit histories. Topics: livecoding annotations, comp4020 sharp tools, RPi kiosk, knowing which Claude is stuck, agentic AI with stones, moving to Cybernetics, dev setup 2026, Opus meets ELIZA. Excluded as AI-co-written: giving-my-livecoding-gigs-a-doi, when-the-work-is-the-output, becoming-a-better-atproto-citizen, shallow-moats-eighteen-months-on, fifty-five-billion-tokens. The six archived op-eds (THE, The Conversation, The Point) were skipped only because their sole commit carries a Claude-Session trailer from the archiving; they are hand-written and are the obvious corpus for a later op-ed context.

Email (candidates-email.md, E1-E10): 5 colleague (strproxy VM request, supervision-EOI decline, talk outline, research-advice reply, THE pitch) and 5 student (crit attendance, late enrolment, thesis draft feedback, HDR proposal feedback, Claude Code seat offer), all 2025-2026 from the anu/phdconvenor accounts, anonymised to role placeholders. The collector read ~50 bodies in detail and found none that looked AI-drafted. Note the ten sent emails are all replies; the forum-DM register (Ed) has no corpus here and email-student.md will stand in for it until one exists.

Academic (candidates-academic.md, A1-A8): 6 clearly hand-written 2012-2016 (why-lc-matters, PhD thesis, liveness, Coding Livecoding CHI, LIVE 2013, JSFI 2015) covering intro, related work, discussion, limitations and conclusion; 2 AI-assisted (ace-26 limitations, llms-unplugged-pedagogy abstract) usable only as befores. cmj-2013 and mad-26 were rejected as reading like a co-author's prose. Verbatim LaTeX is inside code fences so the oxfmt hook leaves it alone.
<!-- SECTION:NOTES:END -->
