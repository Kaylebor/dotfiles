#!/usr/bin/env bash

# Activate mise for tool availability
eval "$(mise activate bash)"

# Update nvim plugins
nvim --headless +PlugInstall +qall

# Update Television community channels
if command -v tv >/dev/null 2>&1; then
    echo "Updating Television community channels..."
    tv update-channels
fi

# Create symlink for carapace specs on macOS
if [[ "$(uname)" == "Darwin" ]] && command -v carapace >/dev/null 2>&1; then
    if [[ ! -L "$HOME/Library/Application Support/carapace/specs" ]]; then
        echo "Creating symlink for carapace specs..."
        mkdir -p "$HOME/Library/Application Support/carapace"
        rm -rf "$HOME/Library/Application Support/carapace/specs"
        ln -s "$HOME/.config/carapace/specs" "$HOME/Library/Application Support/carapace/specs"
    fi
fi

# Note: We're using carapace specs for aider instead of native completions
# due to issues with the generated completion scripts
