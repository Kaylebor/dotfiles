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

## Tool Error Recovery

**CRITICAL**: If you repeatedly fail to use a tool correctly, stop and read `$HOME/CRUSH.md` to verify exact tool signature and usage patterns before trying again. This documentation provides parameter names, types, and examples to prevent repeated tool-calling errors.

## Available Built-in Tools

### File Operations

**`view`** - Read file contents
- **Parameters**: `file_path` (required), `limit` (optional), `offset` (optional)
- **Usage**: `view(file_path="src/main.go", limit=100, offset=50)`
- **Gotchas**: Always provide absolute paths

**`edit`** - Find and replace text in files  
- **Parameters**: `file_path` (required), `old_string` (required), `new_string` (required), `replace_all` (optional)
- **Usage**: `edit(file_path="src/main.go", old_string="foo", new_string="bar")`
- **Gotchas**: `old_string` must match exactly including whitespace

**`write`** - Create or overwrite files
- **Parameters**: `file_path` (required), `content` (required)  
- **Usage**: `write(file_path="src/new.go", content="package main")`
- **Gotchas**: Creates directories if needed

**`multi_edit`** - Multiple file edits in one call
- **Parameters**: `file_path` (required), `edits` (required array)
- **Usage**: `multi_edit(file_path="src/main.go", edits=[{old_string:"foo", new_string:"bar"}])`
- **Gotchas**: Each edit must match exactly

**`ls`** - List directory contents
- **Parameters**: `path` (optional), `max_depth` (optional), `max_items` (optional)
- **Usage**: `ls(path="src", max_depth=2, max_items=50)`
- **Gotchas**: Depth 0 = current directory only

### System Operations

**`bash`** - Execute shell commands
- **Parameters**: `command` (required)
- **Usage**: `bash(command="ls -la && echo done")`
- **Gotchas**: Blocks dangerous commands (package managers, sudo, etc.)

**`download`** - Download files from URLs
- **Parameters**: `url` (required), `file_path` (required), `timeout` (optional)
- **Usage**: `download(url="https://example.com/file.zip", file_path="downloads/file.zip", timeout=300)`
- **Gotchas**: 100MB size limit, max 10min timeout

### Search & Navigation

**`grep`** - Search text content
- **Parameters**: `pattern` (required), `path` (optional), `include` (optional), `exclude` (optional)
- **Usage**: `grep(pattern="function main", path="src/", include="*.go")`
- **Gotchas**: Uses regex patterns

**`glob`** - File pattern matching
- **Parameters**: `pattern` (required), `path` (optional)
- **Usage**: `glob(pattern="**/*.go", path="src/")`
- **Gotchas**: Supports ** wildcards

**`fetch`** - Get web content
- **Parameters**: `url` (required)
- **Usage**: `fetch(url="https://api.example.com/data")`
- **Gotchas**: Returns raw HTML/text, not binary

**`sourcegraph`** - Search code repositories
- **Parameters**: `query` (required)
- **Usage**: `sourcegraph(query="function main repo:owner/name")`
- **Gotchas**: Requires internet access

### Specialized

**`agent`** - Delegate to specialized agents
- **Parameters**: `agent` (required), `prompt` (required)
- **Usage**: `agent(agent="coder", prompt="add error handling")`
- **Agents available**: `coder` (complex tasks), `task` (Q&A)

## Common Tool Call Mistakes

**JSON Structure Errors**:
```json
// WRONG - missing quotes
{file_path: "foo.go"}

// RIGHT  
{"file_path": "foo.go"}
```

**Parameter Name Errors**:
```json
// WRONG - parameter doesn't exist
{"filepath": "foo.go"}

// RIGHT - check exact parameter names
{"file_path": "foo.go"}
```

**Type Errors**:
```json
// WRONG - array instead of string
{"edits": "old_string"}

// RIGHT
{"edits": [{"old_string": "old", "new_string": "new"}]}
```

## Tool System Architecture

**Built-in Tools** (always available):
- Core 12 tools listed above
- Available to all Crush sessions

**LSP Integration** (when configured):
- Provides language server intelligence
- Auto-discovered diagnostics, completions
- File path: `dot_config/crush/crush.json` → `lsp` section

**MCP Integration** (when configured):
- Dynamic tool loading from external servers
- Listed as "Connected: X tools" in TUI
- File path: `dot_config/crush/crush.json` → `mcp` section

**Agent Delegation**:
- `coder` agent - Complex code tasks with file operations
- `task` agent - Simple Q&A and context searches
- Uses different system prompts and tool restrictions

## Quick Reference

**File Editing**: `edit` (simple), `multi_edit` (complex), `write` (new)
**Content Reading**: `view` (files), `fetch` (web), `sourcegraph` (code)
**Search**: `grep` (content), `glob` (patterns), `ls` (directories)
**System**: `bash` (commands), `download` (files), `agent` (specialized)