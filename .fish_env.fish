#!/usr/bin/env fish

mkdir -p ~/.config/fish/conf.d
mkdir -p ~/.config/fish/functions

if not command -q brew
    # Activate brew - check alternative path first, then standard locations
    set -l brew_paths \
        $HOME/homebrew/bin/brew \
        $HOME/.homebrew/bin/brew \
        /opt/homebrew/bin/brew \
        /usr/local/bin/brew \
        /home/linuxbrew/.linuxbrew/bin/brew

    for path in $brew_paths
        if test -x $path
            # Call brew shellenv and convert global exports to universal exports
            # Handle both 'set --global --export'/'set -gx' and 'fish_add_path --global' formats
            eval ($path shellenv \
                | string replace --all -- 'set --global --export' 'set -Ux' \
                | string replace --all -- 'set -gx' 'set -Ux' \
                | string replace --all -- 'fish_add_path --global --move --path' 'fish_add_path -Um' \
                | string replace --all -- 'fish_add_path --global' 'fish_add_path -U')
            break
        end
    end
end

set -U LC_ALL en_US.UTF-8
set -Ux DOCKER_BUILDKIT 1

# Set PATH
set -l paths \
    ~/.local/share/google-cloud-sdk/bin \
    ~/.local/share/yabridge \
    ~/.rd/bin \
    ~/.deno/bin \
    ~/.bun/bin \
    ~/.mix/escripts \
    ~/.local/bin

for path in $paths
    if test -d $path
        fish_add_path -Upm $path
    end
end

# Add GNU utils to path on MacOS
if test (uname) = Darwin; and command -q brew
    # Add GNU utils to path on MacOS
    set -l core_utils coreutils findutils gnu-sed gawk grep
    for util in $core_utils
        set -l libexec_path (brew --prefix $util)"/libexec"
        set -l gnu_path $libexec_path"/gnubin"
        set -l man_path $libexec_path"/gnuman"
        if test -d $gnu_path
            fish_add_path -Upm $gnu_path
        end
        if test -d $man_path
            set -q MANPATH || set -Ux MANPATH ""
            set -Ux MANPATH $man_path $MANPATH
        end
    end
end

# Set EDITOR and VISUAL; EDITOR will have only TUI editors, while VISUAL prefers GUI editors and falls back to TUI editors
set -l tui_editors hx nvim vim vi
if not set -q EDITOR
    if type -q emacsclient
        set -Ux EDITOR 'emacsclient -t'
        # In case Emacs server is not running
        for editor in $tui_editors
            if type -q $editor
                set -Ux EDITOR "$EDITOR --alternate-editor=\"$editor\""
                break
            end
        end
    else
        for editor in $tui_editors
            set -l cmd (string split ' ' $editor)[1]
            if type -q $cmd
                set -Ux EDITOR $editor
                break
            end
        end
    end
end
if not set -q VISUAL
    set -l zed_exec 'zed -w'
    if type -q emacsclient
        set -Ux VISUAL 'emacsclient -c'
        # In case Emacs server is not running
        for editor in $zed_exec $tui_editors
            set -l cmd (string split ' ' $editor)[1]
            if type -q $cmd
                set -Ux VISUAL "$VISUAL --alternate-editor=\"$editor\""
                break
            end
        end
    else
        for editor in $zed_exec $tui_editors
            set -l cmd (string split ' ' $editor)[1]
            if type -q $cmd
                set -Ux VISUAL $editor
                break
            end
        end
    end
end

# I prefer using TUI editors for GIT commits; an exception would be something like Magit, which ignores these environment variables anyway
set -Ux GIT_EDITOR $EDITOR

# Same for PSQL
set -Ux PSQL_EDITOR $EDITOR

# Bat as man pager
set -Ux MANPAGER "sh -c 'sed -u -e \"s/\\x1B\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'"

# Fisher plugin manager
if not functions -q fisher
    curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher update
end

# https://github.com/catppuccin/fzf
set -Ux FZF_DEFAULT_OPTS "\
--color=bg+:#414559,bg:#303446,spinner:#f2d5cf,hl:#e78284 \
--color=fg:#c6d0f5,header:#e78284,info:#ca9ee6,pointer:#f2d5cf \
--color=marker:#babbf1,fg+:#c6d0f5,prompt:#ca9ee6,hl+:#e78284 \
--color=selected-bg:#51576d \
--color=border:#414559,label:#c6d0f5 \
--style minimal \
--preview='preview-smart {}' \
--height 60% --layout reverse --border"

# Also change default fzf find command to use fd instead
set -Ux FZF_DEFAULT_COMMAND "fd -u ."
set -Ux FZF_CTRL_T_COMMAND 'git-aware-files'
set -Ux FZF_CTRL_T_OPTS '--ansi'
set -Ux FZF_ALT_C_COMMAND "fd -u -t d ."

# https://github.com/catppuccin/fish
# Apply theme (automatically answer yes to overwrite prompt)
echo "y" | fish_config theme save "Catppuccin Frappe"

# Configure carapace bridges for shell completion fallback
if type -q carapace
    set -Ux CARAPACE_BRIDGES 'zsh,fish,bash'
end

# Update Fish completions from man pages
# This generates completions for commands that don't have explicit completion files
if type -q fish_update_completions
    echo "Updating Fish completions from man pages..."
    fish_update_completions
end

# Set up gcc paths for alternative Homebrew installations on macOS
if test (uname) = Darwin
    # Check for homebrew in alternative locations
    for brew_prefix in $HOME/homebrew $HOME/.homebrew
        if test -d $brew_prefix/opt/gcc
            # Library paths
            if test -d $brew_prefix/opt/gcc/lib/gcc/current
                set -Ux LIBRARY_PATH "$brew_prefix/opt/gcc/lib/gcc/current:$LIBRARY_PATH"
            end
            
            # C/C++ include paths
            if test -d $brew_prefix/opt/gcc/include
                set -Ux C_INCLUDE_PATH "$brew_prefix/opt/gcc/include:$C_INCLUDE_PATH"
            end
            if test -d $brew_prefix/opt/gcc/include/c++/15
                set -Ux CPLUS_INCLUDE_PATH "$brew_prefix/opt/gcc/include/c++/15:$CPLUS_INCLUDE_PATH"
            end
            
            # Set CC and CXX to use the Homebrew gcc
            if test -x $brew_prefix/bin/gcc-15
                set -Ux CC gcc-15
                set -Ux CXX g++-15
            end
            
            break  # Only use the first found alternative homebrew
        end
    end
end
