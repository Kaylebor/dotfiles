#!/usr/bin/env fish

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
