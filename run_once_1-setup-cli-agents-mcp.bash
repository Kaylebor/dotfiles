#!/usr/bin/env bash

# Setup Claude Code MCP servers
# This runs only once after Claude is installed via mise

# Check if claude command is available
if ! command -v claude &>/dev/null; then
  echo "Claude Code not found in PATH, skipping MCP setup"
  exit 0
fi

# Helper function to add MCP server, ignoring "already exists" errors
add_claude_mcp_server() {
  local server_name="$1"
  local command="$2"

  if claude mcp add -s user "$server_name" -- "$command" 2>&1 | grep -q "already exists in user config"; then
    return 0 # Success, server already exists
  fi
}

add_codex_mcp_server() {
  local server_name="$1"
  local command="$2"
  shift 2 # Discard 2 first arguments, and grab rest
  local args=("$@")

  if codex mcp add "$server_name" "$command" "${args[@]}" 2>&1 | grep -q "Added global MCP server"; then
    return 0
  fi
}

# Claude MCP servers
# Add Context7 MCP server
add_claude_mcp_server "context7" "npx -y @upstash/context7-mcp"
# Add new (experimental) Chrome Dev Tools MCP server
add_claude_mcp_server "chrome-devtools" "npx chrome-devtools-mcp@latest"

# Codex MCP servers
# Add Context7 MCP server
add_codex_mcp_server "context7" "npx" "-y" "@upstash/context7-mcp"
# Add new (experimental) Chrome Dev Tools MCP server
add_codex_mcp_server "chrome-devtools" "npx" "chrome-devtools-mcp@latest"
