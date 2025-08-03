#!/usr/bin/env python3
"""
Context usage monitor hook for Claude Code.
Runs after Claude responds to check context usage and provide warnings.
"""
import json
import sys
import os

def estimate_tokens(text):
    """Rough token estimation: ~4 characters per token"""
    return len(text) / 4

def main():
    # Read the event data from stdin
    try:
        event_data = json.load(sys.stdin)
    except:
        # If no stdin data, exit silently
        return

    # Get transcript from event data
    transcript = event_data.get('transcript', [])
    
    if not transcript:
        return

    # Calculate total character count from all messages
    total_chars = 0
    for msg in transcript:
        if isinstance(msg, dict):
            content = msg.get('content', '')
            if isinstance(content, str):
                total_chars += len(content)
            elif isinstance(content, list):
                # Handle structured content
                for item in content:
                    if isinstance(item, dict) and 'text' in item:
                        total_chars += len(item['text'])

    # Estimate tokens
    estimated_tokens = estimate_tokens(total_chars)
    
    # Context thresholds (Claude 3 has ~150K token context)
    WARNING_THRESHOLD = 100000   # ~67% of context
    CRITICAL_THRESHOLD = 130000  # ~87% of context
    
    # Provide actionable warnings
    if estimated_tokens > CRITICAL_THRESHOLD:
        print("🚨 CRITICAL: Context near limit! (~{:.0f}k tokens)".format(estimated_tokens/1000))
        print("   → Use /compact immediately")
        print("   → Or delegate current task to implementation-agent")
        print("   → Consider using context-manager to extract findings first")
    elif estimated_tokens > WARNING_THRESHOLD:
        print("⚠️  WARNING: Context usage high (~{:.0f}k tokens)".format(estimated_tokens/1000))
        print("   → Consider using context-manager agent to extract findings")
        print("   → Delegate routine tasks to specialized agents")
        print("   → Use /compact if working on new features")
    
    # Always exit successfully (0) to avoid blocking
    sys.exit(0)

if __name__ == "__main__":
    main()