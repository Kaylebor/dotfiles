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
      ~/.local/bin

for path in $paths
  if test -d $path
    fish_add_path -Upm $path
  end
end

if type -q zed
  set ignore_visual true
  set -Ux VISUAL 'zed -w'
else if type -q emacsclient
  set -Ux EDITOR 'emacsclient -t'
  if not set -q ignore_visual
    set -Ux VISUAL 'emacsclient -c'
  end
else if type -q nvim
  set -Ux EDITOR nvim
else if type -q vim
  set -Ux EDITOR vim
else if type -q vi
  set -Ux EDITOR vi
end

if not type -q doom
  fish_add_path -Upm $HOME/.config/emacs/bin
end

# Fisher plugin manager
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher update

# Eza variables from (eza fish plugin)[https://github.com/plttn/fish-eza?tab=readme-ov-file#-configuration]
set -U EZA_STANDARD_OPTIONS --smart-group --header --group-directories-first --hyperlink --time-style=long-iso --icons=always
