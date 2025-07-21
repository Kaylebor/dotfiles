#!/usr/bin/env bash

# Update nvim plugins
nvim --headless +PlugInstall +qall

# Update Television community channels
if command -v tv >/dev/null 2>&1; then
    echo "Updating Television community channels..."
    tv update-channels
fi
