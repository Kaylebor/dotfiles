#!/usr/bin/env fish

mkdir -p ~/.config/fish/conf.d
mkdir -p ~/.config/fish/functions

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

# Activate mise
if command -v mise > /dev/null
  mise activate fish > ~/.config/fish/conf.d/mise.fish
end
# Integrate fzf
if command -v fzf &>/dev/null
  fzf --fish > ~/.config/fish/functions/fzf_key_bindings.fish
end
# Integrate starship
if command -v starship &>/dev/null
  starship init fish > ~/.config/fish/conf.d/starship.fish
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

# Fisher plugin manager
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
