#!/usr/bin/env bash
# ~/.claude/statusline.sh

input=$(cat)

session_name=$(printf '%s' "$input" | jq -r '.session_name // empty')
model_name=$(printf '%s' "$input" | jq -r '.model.display_name // empty')

real_cwd=$(printf '%s' "$input" | jq -r '.cwd // .workspace.current_dir // empty')
cwd="${real_cwd/#$HOME/"~"}"

branch=$(printf '%s' "$input" | jq -r '.workspace.git_worktree // empty')

if [ -z "$branch" ] && [ -n "$real_cwd" ] && [ -d "$real_cwd" ]; then
  branch=$(GIT_OPTIONAL_LOCKS=0 git -C "$real_cwd" symbolic-ref --short HEAD 2>/dev/null)
fi

ctx_pct=$(printf '%s' "$input" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)
five_h=$(printf '%s' "$input" | jq -r '.rate_limits.five_hour.used_percentage // 0' | cut -d. -f1)
five_h_resets_at=$(printf '%s' "$input" | jq -r '.rate_limits.five_hour.resets_at // empty')

five_h_resets=""
if [ -n "$five_h_resets_at" ]; then
  five_h_resets=$(date -r "$five_h_resets_at" +%H:%M:%S 2>/dev/null)
fi

cyan='\033[36m'
green='\033[32m'
yellow='\033[33m'
red='\033[31m'
reset='\033[0m'

make_bar() {
  pct=$1 filled_char=$2 empty_char=$3
  bar_width=10
  filled=$((pct * bar_width / 100))
  [ "$filled" -gt "$bar_width" ] && filled=$bar_width
  empty=$((bar_width - filled))
  printf '%*s' "$filled" '' | tr ' ' "$filled_char"
  printf '%*s' "$empty" '' | tr ' ' "$empty_char"
}

severity_color() {
  pct=$1
  if [ "$pct" -ge 80 ]; then
    printf '%s' "$red"
  elif [ "$pct" -ge 50 ]; then
    printf '%s' "$yellow"
  else
    printf '%s' "$green"
  fi
}

ctx_bar=$(make_bar "$ctx_pct" '▓' '░')
ctx_color=$(severity_color "$ctx_pct")

five_h_bar=$(make_bar "$five_h" '●' '○')
five_h_color=$(severity_color "$five_h")

line1=()

if [ -n "$session_name" ]; then
  line1+=("$(printf "${cyan}[%s]${reset}" "$session_name")")
fi

line1+=("$cwd")

if [ -n "$branch" ]; then
  line1+=("{$branch}")
fi

line2=()

if [ -n "$model_name" ]; then
  line2+=("[$model_name]")
fi

line2+=("$(printf "%b[%s]${reset}" "$ctx_color" "$ctx_bar")")

if [ -n "$five_h_resets" ]; then
  line2+=("5h $(printf "%b[%s]${reset}" "$five_h_color" "$five_h_bar") (resets at $five_h_resets)")
else
  line2+=("5h $(printf "%b[%s]${reset}" "$five_h_color" "$five_h_bar")")
fi

printf '%s\n' "${line1[*]}"
printf '%s\n' "${line2[*]}"
