#!/usr/bin/env bash

# Function to source the mise activation script only if mise is NOT in the PATH
find_and_source_mise_env() {
    if ! command -v mise &> /dev/null; then
        echo "mise not found in PATH, searching for activation script..."

        # Prioritize Homebrew (macOS) and AUR (Arch) paths
        if [ -f "/opt/homebrew/bin/mise" ]; then
            echo "Sourcing mise from /opt/homebrew/bin/mise"
            eval "$(/opt/homebrew/bin/mise activate bash)"
            return 0
        elif [ -f "/usr/bin/mise" ]; then
            echo "Sourcing mise from /usr/bin/mise"
            eval "$(/usr/share/mise activate bash)"
            return 0
        elif [ -e "$HOME/.local/bin/mise" ]; then
          echo "Sourcing mise from $HOME/.local/bin/mise"
          eval "$("$HOME/.local/bin/mise" activate bash)"
        else
            echo "Error: Could not find mise activation script"
            return 1
        fi
    else
        echo "mise found in PATH, no need to source environment"
        return 0
    fi
}

# Attempt to find and source mise environment only if not already in PATH
if find_and_source_mise_env; then
    # Check if mdopen is already installed and install if not
    if [ -z "$(mise exec rust -c 'command -v mdopen')" ]; then
        echo "mdopen not found, installing..."
        mise exec rust -c 'cargo install mdopen'
    else
        echo "mdopen is already installed"
    fi
    # Unconditionally install JS CLI tools
    mise exec bun -c 'bun install -g clipboard-cli firebase-tools typescript-language-server vscode-langservers-extracted @github/copilot-language-server'
else
    echo "Skipping mdopen installation as mise environment was not found or could not be sourced"
fi
