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

You'll also need to install icon fonts for proper display:
```
M-x all-the-icons-install-fonts
M-x nerd-icons-install-fonts
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

### Homebrew Manager CLI

The automation behind these installers lives in `dot_local/bin/chezmoi-homebrew-manager`. The CLI exposes commands such as `full-install`, `check-outdated`, `rebuild-outdated`, and an interactive `mise-refresh` for rebuilding managed Ruby toolchains. Full architecture notes and usage details are documented in `docs/homebrew-manager.md`.

## Shell Completions

This repository uses [carapace](https://carapace-sh.github.io/carapace-bin/) for universal shell completions across Fish, Zsh, and Bash.

### Completion Frameworks

Many modern CLI tools include completion frameworks that provide rich, context-aware completions:

- **Cobra** (Go): Used by `gh`, `kubectl`, `docker`, `helm`, `hugo`
- **Clap** (Rust): Used by `fd`, `rg`, `bat`, `eza`, `difftastic`, `mise`
- **Click** (Python): Used by `pgcli`
- **Argcomplete** (Python): Enhancement for Python's argparse
- **Yargs** (Node.js): Used by many JavaScript CLIs
- **Urfave/cli** (Go): Alternative Go framework

Carapace automatically detects and bridges these frameworks, providing better completions than shell-specific scripts. Framework bridges are preferred because they:
- Get completions directly from the tool's built-in completion engine
- Are faster and more accurate than parsing help text
- Update automatically when the tool updates

The configuration in `.config/carapace/bridges.yaml` maps tools to their frameworks for optimal performance.

## Device-Specific Configuration Management

This repository includes a generalized system for managing JSON configuration files that need both shared settings (managed by Chezmoi) and device-specific settings (preserved per machine).

### How It Works

The system uses a metadata-driven approach:

1. **Configuration metadata** in `.chezmoidata/managed-configs.yaml` defines which files to manage
2. **Template files** contain shared configuration (without device-specific fields)  
3. **Merge script** (`run_onchange_managed-configs.sh.tmpl`) automatically merges templates with existing device settings
4. **Ignored files** in `.chezmoiignore` prevent Chezmoi from directly managing these files

### Supported Applications

Currently manages configurations for:
- **Claude Code** (`~/.claude/settings.json`) - Preserves model preferences and survey state
- **Cursor IDE** (`~/Library/Application Support/Cursor/User/settings.json`) - Preserves UI preferences  
- **Cursor MCP** (`~/.cursor/mcp.json`) - Fully managed (no device-specific fields)
- **Zed Editor** (`~/.config/zed/settings.json`) - Preserves font sizes, theme, agent/model preferences, and context servers

### Key Features

- **Automatic merging**: Template changes trigger automatic merge with existing settings
- **Device preservation**: Existing device-specific fields are never overwritten
- **Interactive updates**: Shows diff preview and asks for confirmation before applying changes
- **Hash-based detection**: Only runs when template content actually changes
- **JSON validation**: Validates templates and existing files, backs up invalid JSON
- **Flexible tooling**: Uses `difft` for enhanced diffs, falls back to standard `diff`

### Adding New Configurations

To manage a new JSON configuration file:

1. **Create template**: Convert existing file to `.tmpl` format, remove device-specific fields
2. **Add to metadata**: Define the configuration in `.chezmoidata/managed-configs.yaml`:
   ```yaml
   my_app_settings:
     template: "dot_config/myapp/settings.json.tmpl"
     destination: ".config/myapp/settings.json"
     preserve_fields:
       - "fontSize"
       - "theme"
     merge_strategy: "shallow"
     description: "My App settings with device-specific preferences"
   ```
3. **Update ignore list**: Add the destination file to `.chezmoiignore`

The system will automatically handle the rest - no need to write custom merge scripts!

### Merge Strategies

- **shallow**: Simple merge where existing values override template values (recommended for most cases)
- **deep**: Recursive merge preserving nested structures while allowing template updates to flow through

## Extras
- Fonts: [Maple Mono](https://github.com/subframe7536/maple-font)
