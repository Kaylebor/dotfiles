#!/usr/bin/env python3
"""
Auto-extract findings hook for Claude Code.
Runs before /compact to save important findings to the knowledge base.
"""
import json
import sys
import os
from datetime import datetime
from pathlib import Path

def get_project_name():
    """Get current project name from working directory"""
    return os.path.basename(os.getcwd())

def extract_key_findings(messages, limit=20):
    """Extract important findings from recent messages"""
    findings = {
        'decisions': [],
        'bugfixes': [],
        'learnings': [],
        'implementations': []
    }
    
    # Analyze recent messages
    recent = messages[-limit:] if len(messages) > limit else messages
    
    for msg in recent:
        if not isinstance(msg, dict):
            continue
            
        role = msg.get('role', '')
        content = msg.get('content', '')
        
        if role == 'assistant' and isinstance(content, str) and len(content) > 200:
            # Look for patterns indicating different types of findings
            content_lower = content.lower()
            
            # Decisions
            if any(word in content_lower for word in ['decided', 'chose', 'selected', 'will use', 'going with']):
                findings['decisions'].append(content[:500])
            
            # Bug fixes
            elif any(word in content_lower for word in ['fixed', 'resolved', 'bug', 'error', 'issue']):
                findings['bugfixes'].append(content[:500])
            
            # Learnings
            elif any(word in content_lower for word in ['learned', 'discovered', 'found that', 'realized']):
                findings['learnings'].append(content[:500])
            
            # Implementations
            elif any(word in content_lower for word in ['implemented', 'created', 'added', 'built']):
                findings['implementations'].append(content[:500])
    
    return findings

def save_findings(findings, project_name):
    """Save extracted findings to knowledge base"""
    knowledge_base = Path.home() / "knowledge" / "projects" / project_name
    knowledge_base.mkdir(parents=True, exist_ok=True)
    sessions_dir = knowledge_base / "sessions"
    sessions_dir.mkdir(exist_ok=True)
    
    timestamp = datetime.now()
    timestamp_str = timestamp.strftime("%Y%m%d_%H%M%S")
    date_str = timestamp.strftime("%Y-%m-%d %H:%M")
    
    # Create session summary
    session_file = sessions_dir / f"session_{timestamp_str}.md"
    
    with open(session_file, 'w') as f:
        f.write(f"# Session Summary - {date_str}\n\n")
        
        if findings['decisions']:
            f.write("## Key Decisions\n")
            for decision in findings['decisions']:
                f.write(f"- {decision.strip()[:200]}...\n")
            f.write("\n")
        
        if findings['bugfixes']:
            f.write("## Bugs Fixed\n")
            for fix in findings['bugfixes']:
                f.write(f"- {fix.strip()[:200]}...\n")
            f.write("\n")
        
        if findings['learnings']:
            f.write("## Learnings\n")
            for learning in findings['learnings']:
                f.write(f"- {learning.strip()[:200]}...\n")
            f.write("\n")
        
        if findings['implementations']:
            f.write("## Implementations\n")
            for impl in findings['implementations']:
                f.write(f"- {impl.strip()[:200]}...\n")
            f.write("\n")
    
    # Also append to categorized files
    if findings['decisions']:
        decisions_file = knowledge_base / "decisions.md"
        with open(decisions_file, 'a') as f:
            if not decisions_file.exists() or decisions_file.stat().st_size == 0:
                f.write("# Project Decisions\n\n")
            f.write(f"\n## Session {date_str}\n")
            for decision in findings['decisions']:
                f.write(f"- {decision.strip()}\n")
    
    if findings['bugfixes']:
        bugfixes_file = knowledge_base / "bugfixes.md"
        with open(bugfixes_file, 'a') as f:
            if not bugfixes_file.exists() or bugfixes_file.stat().st_size == 0:
                f.write("# Bug Fixes\n\n")
            f.write(f"\n## Session {date_str}\n")
            for fix in findings['bugfixes']:
                f.write(f"- {fix.strip()}\n")
    
    return session_file

def main():
    # Read event data from stdin
    try:
        event_data = json.load(sys.stdin)
    except:
        print("✅ No findings to extract")
        return
    
    # Get transcript
    transcript = event_data.get('transcript', [])
    if not transcript:
        print("✅ No conversation to extract from")
        return
    
    # Extract findings
    project_name = get_project_name()
    findings = extract_key_findings(transcript)
    
    # Check if we have any findings
    total_findings = sum(len(f) for f in findings.values())
    if total_findings == 0:
        print("✅ No significant findings to extract")
        return
    
    # Save findings
    session_file = save_findings(findings, project_name)
    
    print(f"✅ Extracted {total_findings} findings to {session_file}")
    print(f"   → Decisions: {len(findings['decisions'])}")
    print(f"   → Bug fixes: {len(findings['bugfixes'])}")
    print(f"   → Learnings: {len(findings['learnings'])}")
    print(f"   → Implementations: {len(findings['implementations'])}")
    
    # Always exit successfully
    sys.exit(0)

if __name__ == "__main__":
    main()