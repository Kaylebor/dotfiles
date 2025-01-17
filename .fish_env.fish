#!/usr/bin/env fish

if type -q zed
  set -Ux VISUAL "zed -w"
end

if type -q emacsclient
  set -Ux EDITOR 'ec'
  set -Ux VISUAL 'ecc'
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
