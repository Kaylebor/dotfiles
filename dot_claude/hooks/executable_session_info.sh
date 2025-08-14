#!/usr/bin/env bash
# Session context information for Claude Code
# Provides system environment details at session start

echo "=== System Environment ==="

# Operating system information
OS="$(uname -s)"
KERNEL="$(uname -r)"
ARCH="$(uname -m)"
echo "OS: $OS $KERNEL on $ARCH"

# Distribution info (Linux only)
if [[ "$OS" == "Linux" ]] && [[ -f /etc/os-release ]]; then
    DISTRO=$(grep -E '^(NAME|VERSION)=' /etc/os-release 2>/dev/null | cut -d'"' -f2 | paste -sd' ' | sed 's/^[[:space:]]*//')
    [[ -n "$DISTRO" ]] && echo "Distro: $DISTRO"
fi

# Shell and terminal environment
echo "Shell: $SHELL"
echo "Terminal: $TERM (${COLUMNS:-?}x${LINES:-?})"

# Current working directory
echo "PWD: $(pwd)"

# SSH connection status
if [[ -n "$SSH_CONNECTION" ]]; then
    SSH_FROM=$(echo "$SSH_CONNECTION" | cut -d' ' -f1)
    echo "SSH: Connected from $SSH_FROM"
fi

# GPU information (concise but informative)
if command -v lspci &>/dev/null; then
    GPU_INFO=$(lspci 2>/dev/null | grep -E '(VGA|3D|Display)' | head -1 | cut -d: -f3 | sed 's/^[[:space:]]*//')
    echo "GPU: ${GPU_INFO:-No discrete GPU detected}"
elif [[ "$OS" == "Darwin" ]]; then
    # macOS system_profiler is slower but more reliable
    GPU_INFO=$(system_profiler SPDisplaysDataType 2>/dev/null | grep -A1 "Chipset Model:" | grep "Chipset Model:" | cut -d: -f2 | sed 's/^[[:space:]]*//' | head -1)
    echo "GPU: ${GPU_INFO:-Unknown GPU}"
fi

# Container runtime detection
CONTAINER_RUNTIMES=()
for cmd in docker podman nerdctl; do
    if command -v "$cmd" &>/dev/null; then
        CONTAINER_RUNTIMES+=("$cmd")
    fi
done

if [[ ${#CONTAINER_RUNTIMES[@]} -gt 0 ]]; then
    echo "Container runtimes: ${CONTAINER_RUNTIMES[*]}"
    
    # Check for running containers (docker first, then others)
    for runtime in "${CONTAINER_RUNTIMES[@]}"; do
        if [[ "$runtime" == "docker" ]] && docker info &>/dev/null; then
            RUNNING=$(docker ps -q 2>/dev/null | wc -l | tr -d ' ')
            if [[ "$RUNNING" -gt 0 ]]; then
                echo "Running containers: $RUNNING (docker)"
                break
            fi
        fi
    done
fi

# Runtime version managers (mise/rtx)
if command -v mise &>/dev/null; then
    TOTAL_TOOLS=$(mise list --current 2>/dev/null | wc -l | tr -d ' ')
    MISE_TOOLS=$(mise list --current 2>/dev/null | head -5 | awk '{printf "%s@%s ", $1, $2}' | sed 's/ $//')
    if [[ -n "$MISE_TOOLS" ]]; then
        echo "Active tools ($TOTAL_TOOLS total): $MISE_TOOLS"
        [[ "$TOTAL_TOOLS" -gt 5 ]] && echo "  → Run 'mise list --current' to see all tools"
    fi
fi