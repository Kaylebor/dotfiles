#!/usr/bin/env python3
"""
Agent suggestion hook for Claude Code.
Analyzes user prompts and suggests appropriate specialized agents.
Runs on UserPromptSubmit to remind Claude about available agents.
"""
import json
import sys
import re

# Agent keywords and patterns
AGENT_PATTERNS = {
    'research-agent': {
        'keywords': ['research', 'find', 'search', 'explore', 'compare', 'evaluate', 'investigate', 
                    'what is', 'how does', 'learn about', 'understand', 'best practices', 'alternatives',
                    'documentation', 'docs', 'options', 'which', 'should i use', 'vs'],
        'patterns': [r'what.*best', r'how.*work', r'find.*library', r'research.*solution', 
                    r'compare.*between', r'learn.*about', r'understand.*how']
    },
    'implementation-agent': {
        'keywords': ['implement', 'code', 'create', 'build', 'write', 'add', 'refactor', 'fix bug',
                    'develop', 'function', 'class', 'module', 'feature', 'update', 'modify', 'change'],
        'patterns': [r'implement.*feature', r'create.*function', r'write.*code', r'add.*to',
                    r'refactor.*module', r'fix.*bug', r'build.*component']
    },
    'test-runner': {
        'keywords': ['test', 'testing', 'check', 'verify', 'validate', 'run tests', 'test suite',
                    'unit test', 'integration test', 'coverage', 'lint', 'typecheck'],
        'patterns': [r'run.*test', r'test.*function', r'check.*work', r'verify.*implementation',
                    r'ensure.*working', r'validate.*changes']
    },
    'doc-writer': {
        'keywords': ['document', 'documentation', 'readme', 'claude.md', 'api docs', 'comments',
                    'explain', 'describe', 'write docs', 'update docs'],
        'patterns': [r'document.*code', r'update.*readme', r'write.*documentation', 
                    r'add.*comments', r'explain.*how']
    },
    'context-manager': {
        'keywords': ['context', 'extract', 'summarize', 'save findings', 'knowledge base',
                    'organize', 'compact', 'token', 'usage'],
        'patterns': [r'extract.*findings', r'save.*knowledge', r'summarize.*work',
                    r'context.*high', r'organize.*information']
    }
}

def analyze_prompt(prompt):
    """Analyze user prompt and suggest appropriate agents"""
    prompt_lower = prompt.lower()
    suggestions = []
    
    # Check each agent's patterns
    for agent, config in AGENT_PATTERNS.items():
        score = 0
        
        # Check keywords
        for keyword in config['keywords']:
            if keyword in prompt_lower:
                score += 1
        
        # Check regex patterns
        for pattern in config['patterns']:
            if re.search(pattern, prompt_lower):
                score += 2
        
        if score > 0:
            suggestions.append((agent, score))
    
    # Sort by score (highest first)
    suggestions.sort(key=lambda x: x[1], reverse=True)
    
    return suggestions

def format_suggestion(agent_name):
    """Format agent suggestion based on type"""
    agent_info = {
        'research-agent': 'for exploring solutions and gathering information',
        'implementation-agent': 'for coding and implementation tasks',
        'test-runner': 'for running tests and validation',
        'doc-writer': 'for documentation updates (token-aware)',
        'context-manager': 'for extracting findings to knowledge base'
    }
    
    return f"Consider using {agent_name} {agent_info.get(agent_name, '')}"

def main():
    # Read event data from stdin
    try:
        event_data = json.load(sys.stdin)
    except:
        return
    
    # Get the user's prompt
    prompt = event_data.get('userPrompt', '')
    if not prompt:
        return
    
    # Analyze prompt for agent suggestions
    suggestions = analyze_prompt(prompt)
    
    if not suggestions:
        return
    
    # If we have strong suggestions (score > 1), provide them
    strong_suggestions = [s for s in suggestions if s[1] > 1]
    
    if strong_suggestions:
        print("🤖 Agent suggestions based on your request:")
        
        # Show top 2 suggestions
        for agent, score in strong_suggestions[:2]:
            print(f"   → {format_suggestion(agent)}")
        
        # Special cases for proactive suggestions
        if 'test' not in prompt.lower() and any(kw in prompt.lower() for kw in ['implement', 'create', 'add', 'fix']):
            print("   → Remember to use test-runner after implementation")
        
        # Context usage reminder
        if len(event_data.get('transcript', [])) > 50:
            print("   → If context is getting high, use context-manager to extract findings")
    
    # Always exit successfully
    sys.exit(0)

if __name__ == "__main__":
    main()