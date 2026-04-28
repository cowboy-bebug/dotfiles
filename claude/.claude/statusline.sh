#!/usr/bin/env bash
# ~/.claude/statusline.sh
# Starship-styled status line for Claude Code.
# Reads session JSON from stdin and prints:
#   [session_name] cwd [branch]
# Colors are ANSI; Claude Code renders them dimmed.

input=$(cat)

# --- session name (bold cyan, shown only when non-empty) -----------------
session_name=$(printf '%s' "$input" | jq -r '.session_name // empty')

# --- current working directory (replace $HOME with ~) --------------------
cwd=$(printf '%s' "$input" | jq -r '.cwd // .workspace.current_dir // empty')
cwd="${cwd/#$HOME/~}"

# --- git branch (skip when on main or when no branch found) --------------
# Prefer workspace.git_worktree; fall back to shelling out to git.
branch=$(printf '%s' "$input" | jq -r '.workspace.git_worktree // empty')

if [ -z "$branch" ]; then
  # Shell out to git using the session cwd so we read the right repo.
  real_cwd=$(printf '%s' "$input" | jq -r '.cwd // .workspace.current_dir // empty')
  if [ -n "$real_cwd" ] && [ -d "$real_cwd" ]; then
    branch=$(GIT_OPTIONAL_LOCKS=0 git -C "$real_cwd" symbolic-ref --short HEAD 2>/dev/null)
  fi
fi

# Suppress branch display when on main (mirrors starship ignore_branches).
[ "$branch" = "main" ] && branch=""

# --- assemble output -----------------------------------------------------
bold_cyan='\033[1;36m'
reset='\033[0m'

parts=()

if [ -n "$session_name" ]; then
  parts+=("$(printf "${bold_cyan}[%s]${reset}" "$session_name")")
fi

parts+=("$cwd")

if [ -n "$branch" ]; then
  parts+=("$(printf "${bold_cyan}{%s}${reset}" "$branch")")
fi

printf '%s\n' "${parts[*]}"
