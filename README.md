# Kaylebor's Dotfiles
This is my dotfiles repository. It is managed by [chezmoi](https://www.chezmoi.io/).

## Prerequisites
- Install [1Password](https://1password.com/downloads) and sign in (avoid Flatpak/Snap versions for now).
- Install [1Password CLI](https://support.1password.com/command-line-getting-started/) and configure it.

## Installation
Execute this command to install chezmoi and apply the dotfiles:
```bash
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply Kaylebor
```

Within Emacs, you may want to run this to install the tree sitter grammars:
```elisp
(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
```

## Extras
- Fonts: [Iosevka and Iosevka Term](https://typeof.net/Iosevka/)
