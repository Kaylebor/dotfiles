# Kaylebor's Dotfiles
This is my dotfiles repository. It is managed by [chezmoi](https://www.chezmoi.io/).

## Prerequisites
- Install [1Password](https://1password.com/downloads) and sign in (avoid Flatpak/Snap versions for now).
- Install [1Password CLI](https://support.1password.com/command-line-getting-started/) and configure it.

### Alternative: Skip 1Password (for MDM environments)
If 1Password CLI is not available or broken (common in MDM setups), you can skip it entirely:

```bash
# Option 1: Set environment variable (disables 1Password CLI and SSH signing)
export CHEZMOI_SKIP_1PASSWORD=true
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply Kaylebor

# Option 2: Provide manual keys (API keys only, git signing will be disabled)
export CHEZMOI_GEMINI_API_KEY="your-api-key"
chezmoi apply
```

**Note**: When 1Password is skipped, git commit signing is automatically disabled to prevent commit failures.

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

## Shell Completions

This repository uses [carapace](https://carapace-sh.github.io/carapace-bin/) for universal shell completions across Fish, Zsh, and Bash.

### Completion Frameworks

Many modern CLI tools include completion frameworks that provide rich, context-aware completions:

- **Cobra** (Go): Used by `gh`, `kubectl`, `docker`, `helm`, `hugo`
- **Clap** (Rust): Used by `fd`, `rg`, `bat`, `eza`, `delta`, `mise`
- **Click** (Python): Used by `pgcli`
- **Argcomplete** (Python): Enhancement for Python's argparse
- **Yargs** (Node.js): Used by many JavaScript CLIs
- **Urfave/cli** (Go): Alternative Go framework

Carapace automatically detects and bridges these frameworks, providing better completions than shell-specific scripts. Framework bridges are preferred because they:
- Get completions directly from the tool's built-in completion engine
- Are faster and more accurate than parsing help text
- Update automatically when the tool updates

The configuration in `.config/carapace/bridges.yaml` maps tools to their frameworks for optimal performance.

## Extras
- Fonts: [Iosevka and Iosevka Term](https://typeof.net/Iosevka/)
