#!/bin/bash

# Script to detect and fix SentinelOne damage to development environment
# Run this when you notice brew/ruby/other tools suddenly missing

set -euo pipefail

echo "🔍 Checking for SentinelOne damage to development environment..."

# Check if brew is missing
if ! command -v brew >/dev/null 2>&1; then
    echo "❌ brew command not found"
    
    # Check if homebrew directory exists but brew is missing
    if [[ -d "$HOME/.homebrew" ]]; then
        echo "⚠️  Homebrew directory exists but brew binary is missing"
        echo "   This is likely SentinelOne quarantine"
        echo "   Try: curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh | bash"
        echo "   Or restore from SentinelOne quarantine"
    else
        echo "⚠️  Homebrew directory missing entirely"
    fi
else
    echo "✅ brew command found"
fi

# Check if Ruby is working
if ! command -v ruby >/dev/null 2>&1; then
    echo "❌ ruby command not found"
    echo "   This is likely SentinelOne quarantine"
    echo "   Try: mise install ruby@3.2.2"
elif ! ruby --version >/dev/null 2>&1; then
    echo "❌ ruby command exists but not working"
    echo "   This is likely SentinelOne interference"
else
    echo "✅ ruby command working"
fi

# Check if mise installations are intact
if [[ -d "$HOME/.local/share/mise/installs" ]]; then
    echo "📦 Checking mise installations..."
    for tool_dir in "$HOME/.local/share/mise/installs"/*; do
        if [[ -d "$tool_dir" ]]; then
            tool_name=$(basename "$tool_dir")
            # Check if the tool's bin directory has executables
            if [[ -d "$tool_dir" ]] && find "$tool_dir" -name "bin" -type d | head -1 | xargs ls 2>/dev/null | grep -q .; then
                echo "   ✅ $tool_name installations present"
            else
                echo "   ❌ $tool_name installations missing executables"
            fi
        fi
    done
fi

# Check for common SentinelOne indicators
if ps aux | grep -i sentinel | grep -v grep >/dev/null 2>&1; then
    echo "🛡️  SentinelOne process detected"
    echo "   Check SentinelOne threat history for quarantined files"
    echo "   Request IT to add exclusions for development directories"
fi

echo ""
echo "🩹 Quick fixes:"
echo "   1. Check SentinelOne Management Console for quarantined files"
echo "   2. Restore quarantined executables if possible"
echo "   3. Reinstall missing tools:"
echo "      - Homebrew: curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh | bash"
echo "      - Ruby: mise install ruby@3.2.2"
echo "      - Other tools: mise install"
echo "   4. Request IT to add exclusions for:"
echo "      - ~/.homebrew/"
echo "      - ~/.local/share/mise/"
echo "      - ~/.cargo/"
echo "      - ~/work/"