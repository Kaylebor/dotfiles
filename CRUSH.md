# CRUSH.md

This file provides guidance to Crush agents when working with code in this repository.

## Repository Overview

Chezmoi-managed dotfiles for cross-platform development (macOS/Arch Linux) using Go templating. This is a sophisticated configuration management system with automated package installation, runtime management, and cross-platform support.

## Essential Commands

**Chezmoi Core**:
- `chezmoi apply` - Deploy changes from source to home directory
- `chezmoi diff` - Preview changes before deployment
- `chezmoi edit <file>` - Edit source files directly
- `chezmoi add --template <path>` - Convert files to templates
- `chezmoi data` - Inspect available template variables
- `chezmoi execute-template <file>` - Render template for verification

**Package Management**:
- `CHEZMOI_REINSTALL_PACKAGES="pkg1,pkg2" chezmoi apply` - Quick reinstall (preserves options)
- `CHEZMOI_FORCE_REINSTALL_PACKAGES="pkg1" chezmoi apply` - Clean reinstall (removes options)
- `dot_local/bin/chezmoi-homebrew-manager` - Advanced Homebrew management CLI

**Development Tools**:
- `mise install` - Install language runtimes
- `bat cache --build` - Rebuild bat syntax cache
- `nvim --headless +PlugInstall +qall` - Install Neovim plugins

## Architecture and Structure

### File Organization
- `dot_*` → `.*` files in home directory (chezmoi naming convention)
- `.tmpl` → Go template files with conditional logic
- `.chezmoidata/` → YAML configuration data (packages, URLs, managed configs)
- `.chezmoitemplates/` → Reusable template partials
- `run_once_*` → Scripts that execute only on first run
- `run_onchange_*` → Scripts that execute when dependencies change

### Platform Detection
Templates use these patterns:
- `.chezmoi.os` - "darwin" or "linux"
- `.chezmoi.osRelease.idLike` - Linux distro family (e.g., "arch")
- `lookPath` - Check if command exists
- `stat` - Check if file/path exists

### Cross-Platform Support
- macOS: Homebrew (standard or alternative installation)
- Linux: Arch with paru AUR helper
- Conditional template blocks: `{{ if eq .chezmoi.os "darwin" }}`

## Package Management System

### Configuration
Package definitions live in `.chezmoidata/packages.yml` with platform-specific sections:
- `darwin.brews` - macOS Homebrew formulae
- `darwin.casks` - macOS applications
- `arch.aur` - Arch Linux AUR packages

### Installation Types
**Standard Homebrew**: `/opt/homebrew` or `/usr/local` with pre-built bottles
**Alternative Homebrew**: `~/.homebrew` for MDM environments (builds from source)

### Advanced Features
- Per-package build flags and environment variables
- Automatic path migration handling
- Outdated package rebuilding
- Custom installation order management

## Template Patterns

### Conditional Logic
```go
{{ if eq .chezmoi.os "darwin" }}
# macOS-specific code
{{ end }}

{{ if lookPath "fish" }}
# Code only if fish is installed
{{ end }}
```

### Prompts and Configuration
```go
{{ $variable := promptString "Enter value" "default" }}
{{ $choice := promptChoice "Choose option" (list "a" "b" "c") }}
{{ $bool := promptBool "Enable feature?" false }}
```

### Path Management
Use pre-calculated paths from `.chezmoi.yaml.tmpl`:
- `.paths.localBin`
- `.paths.configDir`
- `.paths.cacheDir`

## Development Workflow

1. **Edit Source Files**: Always work in `~/.local/share/chezmoi`, never in `$HOME`
2. **Preview Changes**: `chezmoi diff` before applying
3. **Deploy Changes**: `chezmoi apply` to update home directory
4. **Add Packages**: Update `.chezmoidata/packages.yml`
5. **Test Templates**: `chezmoi execute-template <file>` to verify rendering

## Security Considerations

### 1Password Integration
- SSH signing for git commits
- Template data retrieval via `onepasswordRead`
- Bypass available with `CHEZMOI_SKIP_1PASSWORD=true`
- Never display secret values in templates or output

### Keychain Management
macOS login shells guard SSH keychain loading behind `__SSH_KEYCHAIN_LOADED` environment variable to prevent duplicate loading.

## Important Gotchas

### Template Validation
- Always test template changes with `chezmoi execute-template`
- Check for syntax errors before applying
- Verify platform-specific conditionals work on target OS

### Path Resolution
- Use absolute paths or repo-relative paths
- `chezmoi apply_patch` paths are relative to current working directory
- Pre-calculated paths prevent inconsistencies

### Package Installation
- Alternative Homebrew installations require longer build times
- Some packages (gcc, emacs-plus) need special handling on path changes
- Use environment variables for controlled package reinstallation

### Script Execution
- `run_once_*` scripts execute only on initial setup
- `run_onchange_*` scripts trigger when dependencies change
- Scripts use hash tracking to detect configuration changes

## Toolchain Integration

### Language Runtimes
Managed via mise (formerly rtx):
- Ruby, Node, Go, Python, Java (GraalVM)
- Elixir, Erlang, Rust, Deno, Bun

### Editors
- **Emacs** (primary): elpaca package manager, treesit grammars
- **Neovim**: vim-plug package manager
- **Zed**: Configuration in `dot_config/zed/`

### Shell Environment
- **Fish** (primary): Custom functions, completions, abbreviations
- **Zsh**: Antidote plugin manager
- **Nushell**: Cross-platform shell with scripts

### Fuzzy Finders
- **Television** (primary): Channel-based finder
- **fzf** (legacy): Neovim integration only

## Testing and Validation

### Template Testing
```bash
# Test specific template
chezmoi execute-template dot_config/fish/config.fish.tmpl

# Check all template variables
chezmoi data | jq .
```

### Package Validation
```bash
# Check Homebrew installation
{{ .brewBin }} doctor

# Verify AUR packages (Arch)
paru -Q

# Check mise runtimes
mise ls
```

## Troubleshooting

### Common Issues
- **1Password TTY errors**: Ask user to run `chezmoi apply` manually
- **Path migration**: Homebrew packages auto-rebuild on path changes
- **Template syntax**: Use `chezmoi execute-template` for debugging
- **Platform detection**: Verify OS with `chezmoi data | jq '.chezmoi.os'`

### Debug Mode
Set environment variable for verbose output:
```bash
CHEZMOI_VERBOSE=true chezmoi apply
```

## Documentation References

- Internal: `@PACKAGE_MANAGEMENT.md`, `@ONEPASSWORD_SETUP.md`, `@SHELL_COMPLETIONS.md`
- Chezmoi: Context7 Library ID `/twpayne/chezmoi`
- Homebrew Manager: `docs/homebrew-manager.md`