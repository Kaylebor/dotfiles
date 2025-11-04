# CRUSH.md - Global Context for All Crush Sessions

## Memory Management Strategy

**Per-Task Notes**: Use dedicated `CRUSH_{short_description}_NOTES.md` files for each task to avoid conflicts between multiple Crush instances.

**When to Create**: Start each session by creating a task-specific notes file using format `CRUSH_{task}_NOTES.md` where:
- `task` = 2-6 word description of your current work
- Examples: `CRUSH_refactor_backend_NOTES.md`, `CRUSH_debug_api_NOTES.md`, `CRUSH_docs_update_NOTES.md`

**Notes Structure**: Track tools used, important context, task progress, and key decisions made throughout the session.

### Notes Template:
```markdown
# CRUSH_{task}_NOTES.md

## Current Session
**Date**: [auto-update when opening]
**Current Task**: [fill in as you work]

## Available Tools
[Use `view` tool to list current tools and update this section]

## Tools Used This Session
[Document tools as you use them - helps with pattern recognition]

## Key Context & Discoveries
[Important findings, patterns, or critical information]

## Task Progress
[Track major milestones and current status]

---
*Last Updated: [auto-update]*
```

## Tool Awareness Reminder

**Before proceeding**: Always check what tools are available. Use tools explicitly and reference their capabilities. Don't assume limitations - verify what tools you have access to in each interaction.

**Use structured note-taking** for long conversations - maintain task notes to preserve critical information across context window limits.