# CRUSH.md - Global Context for All Crush Sessions

## Memory Management Strategy

**Per-Task Notes**: Use dedicated `CRUSH_{short_description}_NOTES.md` files for each task to avoid conflicts between multiple Crush instances.

**When to Create**: Start each session by creating a task-specific notes file using format `CRUSH_{task}_NOTES.md` where:
- `task` = 2-6 word description of your current work
- Examples: `CRUSH_refactor_backend_NOTES.md`, `CRUSH_debug_api_NOTES.md`, `CRUSH_docs_update_NOTES.md`

**Notes Structure**: Track tools used, important context, task progress, and key decisions made throughout the session.

## Development Guidance

**Test-Driven Development**: When working on a repository, prefer TDD. Write tests before implementation. Not required when debugging system issues or exploratory troubleshooting.

**Answer Formatting**: When communicating with the user, always answer outside thinking sections for clarity.

**Documentation Files**: When user explicitly references a file (especially non-code documentation like spec/CRUSH.md, TESTING.md, etc.):
- **STOP and READ IT COMPLETELY** before proceeding
- **FOLLOW** any references/links to other files recursively
- **TREAT** documented constraints (e.g., "NO REAL TENANT SWITCHING") as hard requirements, not suggestions
- **APPLY** patterns/conventions from documentation before implementing

**Agent Rule Files**: Repository may contain .md files with agent instructions and constraints, located hierarchically throughout the project. Search for them:
- In the current directory and parent directories
- Next to relevant files in subfolders (e.g., `app/services/CLAUDE.md`, `spec/CRUSH.md`)

Common filenames:
- `CRUSH.md` (task-specific Crush rules)
- `AGENTS.md` (general agent guidance)  
- `CLAUDE.md` (Claude-specific instructions)
- `GEMINI.md` (Gemini-specific instructions)

These files contain critical context about project architecture, testing patterns, and hard constraints (e.g., "NO REAL TENANT SWITCHING"). Always read them when referenced.

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

## Mise-Managed Tools Available via Bash

The following tools are installed via Mise and available in the shell environment:

**Essential Dev Tools**:
- **jq/yq** - JSON/YAML processors for data manipulation
- **bat** - Syntax-highlighted file viewer
- **fd** - Fast, user-friendly file finder
- **ripgrep (rg)** - Fast recursive search
- **eza** - Modern `ls` replacement
- **zoxide** - Smart directory navigation (`z` command)
- **zellij** - Terminal multiplexer and workspace manager
- **fzf** - Fuzzy finder for interactive search
- **hyperfine** - Command-line benchmarking tool

**Language Runtimes**:
- **node**, **bun**, **deno** - JavaScript/TypeScript runtimes
- **python** - Python interpreter with pipx/uv package managers
- **go** - Go toolchain (includes goimports, gopls, gotests)
- **rust** - Rust toolchain with cargo
- **java** - GraalVM Java distribution
- **ruby** - Ruby interpreter
- **lua** - Lua scripting language
- **elixir/erlang** - BEAM ecosystem languages

**Data & Media**:
- **ffmpeg** - Video/audio processing
- **jc** - Convert command output to JSON
- **gron** - Transform JSON into grep-friendly format
- **xan** - CSV toolkit and visualization

**Development Utilities**:
- **gh** - GitHub CLI
- **gitu** - Magit-inspired Git TUI
- **difftastic** - Syntax-aware diff tool
- **shellcheck** - Shell script linter
- **prettier** - Code formatter for web languages
- **biome** - JS/TS formatter and linter
- **golangci-lint** - Go linters aggregator
- **staticcheck** - Go static analysis
- **lefthook** - Git hooks manager
- **claude/codex** - AI coding assistants
- **stylua** - Lua code formatter
- **usage** - CLI specification toolkit
- **mermaid-cli** - Diagram rendering from CLI

**Network & Security**:
- **curlie** - HTTP client with HTTPie-style syntax
- **age** - Modern encryption tool
- **ctop** - Container metrics monitor

**Documentation & Preview**:
- **glow** - Markdown pager and renderer
- **go-grip** - GitHub-style Markdown preview server
- **mdopen** - Open Markdown in browser
- **lnav** - Log file navigator