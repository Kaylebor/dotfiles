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

# Generate aider zsh completions
if command -v aider >/dev/null 2>&1; then
    # Generate completion once
    AIDER_COMPLETION=$(aider --shell-completions zsh)
    
    # Determine the appropriate site-functions directory
    if [[ -d "/usr/local/share/zsh/site-functions" ]] && [[ -w "/usr/local/share/zsh/site-functions" ]]; then
        # Standard location that's writable
        SITE_FUNCTIONS="/usr/local/share/zsh/site-functions"
    elif [[ -d "$HOME/.local/share/zsh/site-functions" ]]; then
        # User-level fallback
        SITE_FUNCTIONS="$HOME/.local/share/zsh/site-functions"
    else
        # Create user-level directory if nothing else works
        SITE_FUNCTIONS="$HOME/.local/share/zsh/site-functions"
        mkdir -p "$SITE_FUNCTIONS"
    fi
    
    echo "Installing aider completion to $SITE_FUNCTIONS"
    echo "$AIDER_COMPLETION" > "$SITE_FUNCTIONS/_aider"
    
    # Also keep it in our custom directory for interactive shells
    ZSH_COMPLETIONS_DIR="$HOME/.config/zsh/completions"
    mkdir -p "$ZSH_COMPLETIONS_DIR"
    echo "$AIDER_COMPLETION" > "$ZSH_COMPLETIONS_DIR/_aider"
fi
