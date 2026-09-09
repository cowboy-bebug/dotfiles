#!/usr/bin/env bash
# PreToolUse hook (Edit|Write): denies edits that introduce new code comments,
# per the user's global CLAUDE.md rule ("Don't add comments unless absolutely
# necessary"). Only flags NEW lines (diffed against the prior content), so
# untouched pre-existing comments never block unrelated edits. Comment syntax
# is picked per file extension/basename since it varies across languages.
set -euo pipefail

input=$(cat)

tool_name=$(jq -r '.tool_name' <<<"$input")
file_path=$(jq -r '.tool_input.file_path // empty' <<<"$input")

allow() { exit 0; }

[ -z "$file_path" ] && allow

base=$(basename -- "$file_path")
name="${base#.}"
if [[ "$name" == *.* ]]; then
  ext="${base##*.}"
else
  ext=""
fi

line_pat=""
block_open=""
block_cont=""

case "$base" in
  Dockerfile|Makefile|makefile|Rakefile|Gemfile|Vagrantfile|Procfile| \
  .curlrc|.gitignore|.zprofile|.zshenv|.zshrc)
    line_pat='(^|\s)#(?!\{)' ;;
esac

if [ -z "$line_pat" ]; then
  case "$ext" in
    ts|tsx|js|jsx|mjs|cjs|go|rs|java|c|h|cpp|hpp|cc|cxx|hxx|cs|swift|kt|kts|scala|php|dart|m|mm|proto|groovy|zig)
      line_pat='(?<!:)//'
      block_open='/\*'
      block_cont='^\s*\*(?!/)'
      ;;
    css|scss|less)
      block_open='/\*'
      block_cont='^\s*\*(?!/)'
      ;;
    py|rb|sh|bash|zsh|fish|pl|pm|yaml|yml|toml|r|jl|nim|ex|exs|coffee|ps1|conf|cfg|ini|tf|tfvars)
      line_pat='(^|\s)#(?!\{)'
      ;;
    sql)
      line_pat='(^|\s)--'
      block_open='/\*'
      block_cont='^\s*\*(?!/)'
      ;;
    lua|hs|elm)
      line_pat='(^|\s)--'
      ;;
    lisp|clj|cljs|cljc|el|scm|rkt)
      line_pat='(^|\s);'
      ;;
    html|htm|xml|vue|svelte|md|mdx)
      block_open='<!--'
      ;;
    *)
      allow ;;
  esac
fi

if [ "$tool_name" = "Write" ]; then
  new_content=$(jq -r '.tool_input.content // empty' <<<"$input")
  old_content=""
  [ -f "$file_path" ] && old_content=$(cat "$file_path")
elif [ "$tool_name" = "Edit" ]; then
  new_content=$(jq -r '.tool_input.new_string // empty' <<<"$input")
  old_content=$(jq -r '.tool_input.old_string // empty' <<<"$input")
else
  allow
fi

old_file=$(mktemp)
new_file=$(mktemp)
trap 'rm -f "$old_file" "$new_file"' EXIT

printf '%s\n' "$old_content" | sort -u >"$old_file"
printf '%s\n' "$new_content" | sort -u >"$new_file"

added_lines=$(comm -13 "$old_file" "$new_file")

patterns=()
[ -n "$line_pat" ] && patterns+=(-e "$line_pat")
[ -n "$block_open" ] && patterns+=(-e "$block_open")
[ -n "$block_cont" ] && patterns+=(-e "$block_cont")

directive_pattern='^#!|^\s*#\s*-\*-|eslint-disable|eslint-enable|ts-ignore|ts-expect-error|ts-nocheck|@ts-|prettier-ignore|istanbul ignore|c8 ignore|type:\s*ignore|noqa|pylint:\s*disable|pylint:\s*enable|pragma|nolint|nosec|rubocop:disable|rubocop:enable'

violations=$(printf '%s\n' "$added_lines" | rg -P "${patterns[@]}" 2>/dev/null | rg -Pv -e "$directive_pattern" || true)

if [ -n "$violations" ]; then
  reason=$(printf 'New code comment(s) added, blocked by the "no comments unless absolutely necessary" rule (~/.claude/CLAUDE.md). A comment only earns its place by explaining a non-obvious WHY (hidden constraint, gotcha, invariant), never restating WHAT the code does. If none of the added lines meet that bar, retry without them. If one genuinely does, keep only that one line and drop the rest.\n\nFlagged line(s):\n%s' "$violations")
  jq -n --arg reason "$reason" '{
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason: $reason
    }
  }'
fi

exit 0
