# Kaylebor's Dotfiles
This is my dotfiles repository. It is managed by [chezmoi](https://www.chezmoi.io/).

## Prerequisites
- Install [1Password](https://1password.com/downloads) and sign in (avoid Flatpak/Snap versions for now).
- Install [1Password CLI](https://support.1password.com/command-line-getting-started/) and configure it.

### Alternative: Skip 1Password (for MDM environments)
If 1Password CLI is not available or broken (common in MDM setups), you can skip it entirely:

```bash
# Option 1: Set environment variable
export CHEZMOI_SKIP_1PASSWORD=true
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply Kaylebor

# Option 2: Provide manual keys
export CHEZMOI_GIT_SIGNING_KEY="ssh-ed25519 AAAAC3..."
export CHEZMOI_GEMINI_API_KEY="your-api-key"
chezmoi apply
```

## Installation
Execute this command to install chezmoi and apply the dotfiles:
```bash
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply Kaylebor
```

Within Emacs, you may want to run this to install the tree sitter grammars:
```elisp
(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
```

## Package Management

### Homebrew Installation Types
This repository supports two Homebrew installation approaches:

#### Standard Installation
- **Location**: `/opt/homebrew` (Apple Silicon) or `/usr/local` (Intel)
- **Benefits**: Uses pre-built bottles, faster installation
- **Use case**: Normal environments without restrictions

#### Alternative Installation  
- **Location**: `~/.homebrew` 
- **Benefits**: Works in MDM environments where system directories are restricted
- **Trade-offs**: All packages built from source, longer installation times
- **Special handling**: Some packages (like gcc) require additional build flags for compatibility
- **Known issues**: GCC may show post-install dylib fixing errors but functions correctly

The installation type is configured during `chezmoi init` and affects package compilation and paths automatically.

### Reinstalling Packages

When updating packages (especially those with custom build options like `emacs-plus`), you can use environment variables to control reinstallation:

#### Quick Reinstall (preserves options)
```bash
CHEZMOI_REINSTALL_PACKAGES="neovim,tmux" chezmoi apply
```
- Uses `brew reinstall` under the hood
- Preserves installation options
- Good for fixing corrupted installations

#### Force Reinstall (clean installation)
```bash
CHEZMOI_FORCE_REINSTALL_PACKAGES="emacs-plus@31" chezmoi apply
```
- Uninstalls then reinstalls the package
- Removes all options, permissions, and metadata
- Required when changing installation options
- Recommended for packages like `emacs-plus` that need complete removal on updates

You can specify multiple packages separated by commas, and use both variables together if needed.

## Extras
- Fonts: [Iosevka and Iosevka Term](https://typeof.net/Iosevka/)
