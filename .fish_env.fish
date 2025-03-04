#!/usr/bin/env fish

mkdir -p ~/.config/fish/conf.d
mkdir -p ~/.config/fish/functions

if not command -q brew
  # Activate brew
  set -l brew_paths \
      /opt/homebrew/bin/brew \
      /usr/local/bin/brew \
      /home/linuxbrew/.linuxbrew/bin/brew

  for path in $brew_paths
    if test -x $path
      eval (parse_shellenv $path shellenv | string replace -- ' --path' '' | string replace -- --global -U | source)
      break
    end
  end
end

set -U LC_ALL en_US.UTF-8

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
if test (uname) = "Darwin"
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
set -l tui_editors helix hx nvim vim vi
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

# Add Doom to PATH; may delete this later as I am migrating to bare Emacs with a custom init.el
if not type -q doom and test -d $HOME/.config/emacs/bin
  fish_add_path -Upm $HOME/.config/emacs/bin
end

# Bat as man pager
set -Ux MANPAGER "sh -c 'sed -u -e \"s/\\x1B\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'"

# Fisher plugin manager
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher update

# Eza variables from (eza fish plugin)[https://github.com/plttn/fish-eza?tab=readme-ov-file#-configuration]
set -U EZA_STANDARD_OPTIONS --smart-group --header --group-directories-first --hyperlink --time-style=long-iso --icons=always
