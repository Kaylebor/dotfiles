#!/usr/bin/env python3
"""
Smart tool-based agent suggestions for Claude Code.
Analyzes tool usage patterns and suggests appropriate follow-up agents.
"""
import json
import sys

def suggest_based_on_tool(tool_name, tool_params):
    """Suggest agents based on the tool that was just used"""
    suggestions = []
    
    # File modification tools
    if tool_name in ['Write', 'Edit', 'MultiEdit']:
        file_path = tool_params.get('file_path', '')
        
        # Suggest test-runner for code files
        if any(ext in file_path for ext in ['.py', '.js', '.ts', '.go', '.rs', '.java', '.rb', '.erb']):
            suggestions.append("💡 Consider using test-runner agent to validate these changes")
        
        # Suggest doc-writer for documentation
        if 'README' in file_path or '.md' in file_path:
            suggestions.append("📝 Consider using doc-writer agent to update related docs")
            
        # For CLAUDE.md specifically
        if 'CLAUDE.md' in file_path:
            suggestions.append("⚠️  Remember: CLAUDE.md has a 2000 token limit. Use doc-writer for token-aware editing")
    
    # Reading/searching tools
    elif tool_name in ['Read', 'Grep', 'Glob']:
        suggestions.append("🔍 If you need more context, use research-agent for comprehensive exploration")
    
    # Bash commands
    elif tool_name == 'Bash':
        command = tool_params.get('command', '')
        
        # Git operations
        if 'git' in command:
            suggestions.append("📋 Consider using context-manager to save important findings before commits")
        
        # Test commands
        if any(test_cmd in command for test_cmd in ['test', 'pytest', 'jest', 'cargo test']):
            suggestions.append("✅ Good! Following up with test-runner ensures comprehensive validation")
    
    return suggestions

def main():
    # Read event data from stdin
    try:
        event_data = json.load(sys.stdin)
    except:
        return
    
    # Get tool information
    tool_name = event_data.get('toolName', '')
    tool_params = event_data.get('toolParameters', {})
    
    if not tool_name:
        return
    
    # Get suggestions based on tool usage
    suggestions = suggest_based_on_tool(tool_name, tool_params)
    
    # Print suggestions if any
    for suggestion in suggestions:
        print(suggestion)
    
    # Check conversation length for context management
    transcript_length = len(event_data.get('transcript', []))
    if transcript_length > 100:
        print("📊 Long conversation detected. Consider using context-manager to extract findings")
    
    sys.exit(0)

if __name__ == "__main__":
    main()