---
name: daily-slack-digest
description: On Claude Code session start, surface yesterday's #team-whisper activity as session context. Reads today's section from a local org file if already written, otherwise summarises yesterday's Slack and prepends a new dated heading.
---

# Daily Slack digest for the Whisper team

Runs from a user-level SessionStart hook. The first Claude Code session of the day summarises yesterday's #team-whisper activity into a local org file and surfaces it as session context. Every subsequent session that day just reads the already-written section back.

## Destination

Append to the org file:

```
~/github.com/cowboy-bebug/org/notes/20260423T091749--daily-slack-digest__agent_ezyvet_idexx_whisper.org
```

Denote-style — keep the existing `#+title` / `#+date` / `#+filetags` / `#+identifier` header intact. Each new daily section is prepended directly after the header (newest-first reading order).

## Source

- **Slack channel:** `#team-whisper`, id `C0ATYD5263C`.
- **Time window:** the previous local calendar day (00:00 to 23:59 NZT / Pacific/Auckland). yesterday = `today − 1 day`.
- Channel messages only — do NOT follow thread replies.

## Decision logic

1. Compute `today` as `YYYY-MM-DD` in local NZT.
2. Read the org file.
3. If the file already contains a heading `* <today>`, extract its section body and inject as session context via `hookSpecificOutput.additionalContext`. Exit silently — do NOT re-summarise, do NOT edit the file.
4. Otherwise, read yesterday's channel messages in range and produce a new `* <today>` section summarising the **previous day**'s activity.
5. Insert the new section directly after the Denote metadata header, write the file, and inject the section as session context.

## Org section format

```org
* 2026-04-23
/Summary of #team-whisper activity from 2026-04-22 (local NZT)./

** Done
- Short sentence. Who did what. [[https://idexx.enterprise.slack.com/archives/C0ATYD5263C/p...][link]]

** In progress
- What's being worked on, who's driving, and the expected next step.

** Blocking
- The blocker, who's waiting on what, and who needs to unblock it.
```

Tight bullets — 2–5 per bucket, one sentence each. Link originating Slack message(s) with the deep-link format `https://idexx.enterprise.slack.com/archives/C0ATYD5263C/p<TS without dot>`.

Omit a bucket's sub-heading entirely if there's nothing in it. If the whole day had no substantive activity, the section can be a one-liner:

```org
* 2026-04-23
/No substantive activity; channel was quiet or off-topic./
```

## What goes in which bucket

- **Done** — completed work, merges/deploys announced, decisions reached, blockers removed.
- **In progress** — work currently underway, ongoing investigations, conversations still open, items where the next step is known but pending.
- **Blocking** — anything waiting on a person, external team, decision, access, or missing info. Be explicit about _who_ is blocked on _what_ and who needs to unblock it.

Social chatter, introductions, standup pleasantries, calendar invites, and "nice work!" reactions are **not** substantive. Drop them.

## Safety

- Redact secrets (tokens, keys, JWTs). If one appears, replace with `<redacted>`.
- Don't include raw customer/PII data even if it was pasted.
- Internal team names, repo names, Jira IDs, and internal URLs are fine.

## Failure handling

If any MCP call fails (Slack unauth, rate limit) or the org file is unreachable: log to stderr and exit 0. Do NOT block the session start. Do NOT surface the error into the main chat.

If the Slack MCP is connected to a workspace that doesn't contain `C0ATYD5263C` (enterprise-grid mismatch), the read will 404 with `channel_not_found` — treat this like any other MCP failure: log, exit 0.
