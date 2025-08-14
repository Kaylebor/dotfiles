#!/usr/bin/env bash
# Per-prompt context information for Claude Code
# Provides current time and git status (fast execution)

# Current date and time with timezone
echo "Today's date: $(date '+%A, %B %d, %Y at %I:%M %p %Z')"

# Git repository status (only if in a git repo)
if git rev-parse --git-dir &>/dev/null 2>&1; then
    BRANCH=$(git branch --show-current 2>/dev/null)
    CHANGES=$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ')
    
    if [[ -n "$BRANCH" ]]; then
        if [[ "$CHANGES" -eq 0 ]]; then
            echo "Git: $BRANCH (clean)"
        else
            echo "Git: $BRANCH ($CHANGES uncommitted changes)"
        fi
    fi
fi