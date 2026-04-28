# User-level instructions

## Writing style

- **Do NOT use em dashes (—)** in any output: chat responses, file contents
  (org, markdown, code comments), commit messages, PR descriptions. Substitute
  with commas, colons, semicolons, parentheses, or split into two sentences.
- **Respect 80-column line width** when writing prose into files (org notes,
  markdown, code comments, commit-message bodies). Hard-wrap at or before 80.
  Does not apply to code or data that would break if wrapped (long URLs, SQL
  one-liners, base64 strings, test-data lines).

## Git commits

- **Do NOT append a `Co-Authored-By: Claude ...` trailer** (or any Claude
  attribution) to commit messages. This applies to every repo, on every project.

## Personal documentation project

My personal documentation project lives at `~/github.com/cowboy-bebug/org`.

- `todo.org` holds all my TODO items. Work-related items are tagged `work`
  and personal items are tagged `personal`.
- `~/github.com/cowboy-bebug/org/logs` holds occasional work logs recorded
  via `org-journal`.
- `~/github.com/cowboy-bebug/org/notes` holds all my notes, captured via
  `denote`.

Notable files for my current work:

- `~/github.com/cowboy-bebug/org/notes/20251020T094708--ezyvet__idexx.org`
  captures all ezyvet-related notes, including database queries, AWS
  commands, code snippets, and APIs (via `restclient`).
- `~/github.com/cowboy-bebug/org/notes/20251010T123533--ezycab__idexx_maximus.org`
  captures all ezycab-related notes (same shape as the ezyvet file).
