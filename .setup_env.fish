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

# Prompt
if command -q starship
  # Integrate starship
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
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher update
