#!/usr/bin/env fish

if test -x (command -v zed)
  set -Ux VISUAL "zed -w"
end

if test -x (command -v nvim)
  set -Ux EDITOR nvim
else if test -x (command -v vim)
  set -Ux EDITOR vim
else if test -x (command -v vi)
  set -Ux EDITOR vi
end
