# CRUSH.md - Global Context for All Crush Sessions

## Memory Management Strategy

**Per-Task Notes**: Use dedicated `CRUSH_{short_description}_NOTES.md` files for each task to avoid conflicts between multiple Crush instances; keep track of them in `CRUSH_INDEX.md` in the same folder.
These files are added to the global `.gitignore` file and should be used for local-only task tracking.

**When to Create**: Start each session by creating a task-specific notes file using format `CRUSH_{task}_NOTES.md` where:
- `task` = 2-6 word description of your current work
- Examples: `CRUSH_refactor_backend_NOTES.md`, `CRUSH_debug_api_NOTES.md`, `CRUSH_docs_update_NOTES.md`

**Notes Structure**: Track tools used, important context, task progress, and key decisions made throughout the session.

## Development Guidance

**Priorities**: Prefer TDD on repositories. Answer concisely outside thinking sections.

**Documentation Files**: When user references a file (especially `spec/CRUSH.md`, `TESTING.md`):
- Read it completely first
- Follow all references recursively  
- Treat constraints (e.g., "NO REAL TENANT SWITCHING") as hard requirements
- Apply patterns before implementing

**Agent Rules**: Check for `.md` instruction files in current/parent directories and near relevant files:
`CRUSH.md`, `AGENTS.md`, `CLAUDE.md`, `GEMINI.md`

**Library Research**: 1) `mcp_docs_*` (docs), 2) sourcegraph (implementation), 3) `mcp_www_*` (general info)

**MCP Tools**: Prefixed with `mcp_`. Conditionally available: `docs` (context7 MCP server), `www` (Brave Search MCP server), SonarQube, ZAI.

### Notes Template
```markdown
# CRUSH_{task}_NOTES.md
**Date**: [auto-update]
**Current Task**: [fill in]

## Tools Used
[List as you go - helps pattern recognition]

## Key Context
[Findings, patterns, critical info]

## Progress
[Milestones and status]
```

**Tool Awareness**: Check available tools each session. Don't assume - verify.

## Tool Usage Preferences

### General Principles

**Use agents proactively**: They keep global context clean and avoid frequent summarizations that lose information. Delegate complex subtasks to `coder` agent.

**Never ask questions in thinking mode**: All user interactions, questions, and requests for clarification must happen in non-thinking mode. The thinking section is for internal reasoning only.

**Built-in vs bash**: Prefer built-in versions unless bash offers unique capabilities:
- `fetch/download` instead of curl/wget
- `glob` instead of find
- `grep` instead of grep via bash
- `ls` instead of ls via bash  
- `edit/multiedit/write` instead of sed
- `view` instead of cat

**Mise tools in bash**: Many powerful tools available via Mise (jq, yq, fd, rg, bat, etc.). Often easier/better than built-ins for complex operations.

### Built-in Tools

**When to use what:**
- `agentic_fetch` - Complex pages where you need specific data extracted
- `fetch` - Simple raw content retrieval
- `glob` → `grep` - Find files first, then search within them
- `sourcegraph` - Library source code (check local version first, then latest online)
  - Do NOT use sourcegraph for local repo search
- `view` → `edit/multiedit` - Always read before editing
- `bash` with `run_in_background` - Long-running servers, watch tasks, continuous processes

### MCP Tools (when available)

**context7**: Library documentation - tends to be more useful than web search for established patterns

**Brave Search**: 
- Web/news search when research direction is still forming
- General troubleshooting and community patterns

**SonarQube**: Code quality/security analysis when available

**ZAI**: Image/video analysis for understanding visual content

**LSP**: Use when available - excellent for codebase navigation

## Conditionally Available MCP Tools

Require external MCP servers. Prefixed with `mcp_`. Main categories:

**Context7**: Library documentation lookup. Has two functions: ID search, then docs search with that ID. Cache important library IDs in task notes or here to avoid repeated searches.

**Brave Search**: Web, news, image, video, local search + AI summarizer

**SonarQube**: Code issues, security hotspots, quality metrics

**ZAI**: Image/video analysis via AI vision

## Tool Syntax

**Correct**:
```json
{"file_path": "foo.go", "old_string": "foo", "new_string": "bar"}
{"edits": [{"old_string": "a", "new_string": "b"}]}
```

**Common errors**:
- Missing quotes on keys: `{file_path: "foo.go"}`
- Wrong parameter names: `filepath` instead of `file_path`
- Type mismatches: string instead of array for `edits`

## Tool System

**Built-in**: Always available (listed above)

**LSP Integration** (when configured): Language server intelligence via `dot_config/crush/crush.json`

**MCP Integration** (when configured): External tools loaded dynamically; prefixed with `mcp_`

## Quick Reference

**Edit**: `edit` (simple), `multiedit` (complex), `write` (new)
**Read**: `view` (files), `fetch` (web), `sourcegraph` (code)
**Search**: `grep` (content), `glob` (patterns), `ls` (directories)
**System**: `bash` (commands), `download` (files), `agent` (specialized)

## Shell Tools (via Mise)

Available in bash shell: jq/yq, bat, fd, rg, eza, zoxide, fzf, gh, difftastic, shellcheck, node/bun/deno, python, go, rust, java, ruby, ffmpeg, and more.
