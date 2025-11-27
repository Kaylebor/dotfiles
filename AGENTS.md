# AGENTS.md

Guidance for automated coding agents working inside this chezmoi-managed dotfiles repo.

## CRITICAL: Chezmoi is a TOOL, NOT this repository

**Chezmoi** is the configuration management tool we USE to manage dotfiles. This repository CONTAINS chezmoi-managed configuration files (templates, scripts, data). 

- **Chezmoi** = External Go-based dotfile manager (https://github.com/twpayne/chezmoi)
- **This repo** = Dotfiles and templates managed BY chezmoi

Always distinguish between:
- The chezmoi tool/CLI (external dependency)
- This chezmoi repository (the dotfiles being managed)

## Long-Term Memory & Precedence

**AGENTS.md** is this project's primary long-term memory. **CRUSH.md** documents local Crush CLI preferences and global patterns, but **AGENTS.md takes precedence** for repository-specific decisions and patterns.

When guidance conflicts:
1. **AGENTS.md** (this file) - Repository-specific rules WIN
2. **CRUSH.md** - Global Crush patterns and preferences
3. **CLAUDE.md** - General Claude guidance

Document all important repository learnings here for persistence across sessions.

## Core Workflows

**File Location Rule (CRITICAL)**: Always edit sources under `~/.local/share/chezmoi` (this repo), NEVER in `$HOME` directly. Changes made in `$HOME` will be overwritten on next `chezmoi apply`.

**Standard Workflow**:
1. **Edit**: Make changes in `~/.local/share/chezmoi/`
2. **Preview**: `chezmoi diff` to review changes before deploying
3. **Deploy**: `chezmoi apply` to update home directory
4. **Test**: `chezmoi execute-template <file>` to verify template rendering
5. **Add Packages**: Update `.chezmoidata/packages.yml` for new software

**Editing Approach**:
- Before running commands, confirm active OS: `chezmoi data | jq '.chezmoi.os'` (supports macOS and Arch Linux)

## AI-Assisted Editing Guidelines

### Working with AI Tools
When using AI-assisted editing capabilities in this chezmoi repository.

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
- **macOS**: Homebrew (standard or alternative installation)
- **Linux**: Arch with paru AUR helper
- Conditional template blocks: `{{ if eq .chezmoi.os "darwin" }}`

## Package Management System

### Configuration
Package definitions live in `.chezmoidata/packages.yml` with platform-specific sections:
- `darwin.brews` - macOS Homebrew formulae
- `darwin.casks` - macOS applications
- `arch.aur` - Arch Linux AUR packages

### Installation Types
- **Standard Homebrew**: `/opt/homebrew` or `/usr/local` with pre-built bottles
- **Alternative Homebrew**: `~/.homebrew` for MDM environments (builds from source)

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

## Security Considerations

### 1Password Integration
- SSH signing for git commits
- Template data retrieval via `onepasswordRead`
- Bypass available with `CHEZMOI_SKIP_1PASSWORD=true`
### Keychain Management (macOS)
Login shells (.profile, .zprofile, fish login, Nushell login) guard `ssh-add --apple-load-keychain` behind the `__SSH_KEYCHAIN_LOADED` environment variable so the call runs at most once per login chain.

## Important Gotchas

### Template Validation
- Always test template changes with `chezmoi execute-template`
- Check for syntax errors before applying
- Verify platform-specific conditionals work on target OS

### Path Resolution
- Use absolute paths or repo-relative paths
- Some chezmoi commands (e.g., apply_patch) use paths relative to current working directory
- Pre-calculated paths prevent inconsistencies

### Package Installation
- Alternative Homebrew installations require longer build times
- Some packages (gcc, emacs-plus) need special handling on path changes
- Use environment variables for controlled package reinstallation

### Script Execution
- `run_once_*` scripts execute only on initial setup
- `run_onchange_*` scripts trigger when dependencies change
- Scripts use hash tracking to detect configuration changes

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
- **1Password TTY errors**: Some AI tools cannot run chezmoi with 1Password when TTY is unavailable. If `chezmoi apply` fails with 1Password errors related to TTY availability, ask the user to run the command manually in their terminal.
- **Path migration**: Homebrew packages auto-rebuild on path changes
- **Template syntax**: Use `chezmoi execute-template` for debugging
- **Platform detection**: Verify OS with `chezmoi data | jq '.chezmoi.os'`

### Debug Mode
Set environment variable for verbose output:
```bash
CHEZMOI_VERBOSE=true chezmoi apply
```

## Toolchain Integration

### Fuzzy Finders
- **Television** (primary): Modern channel-based finder with `tv init`/`tv update-channels`
- **fzf** (legacy): Kept for Neovim integration only

### Shell Completions
- **Carapace**: Universal completion engine supporting Fish, Zsh, Bash, and more

See @SHELL_COMPLETIONS.md for framework bridges, configuration, and usage details

## References

- **Chezmoi Docs**: Context7 Library ID `/twpayne/chezmoi`
- **Internal Docs**: See `@PACKAGE_MANAGEMENT.md`, `@ONEPASSWORD_SETUP.md`, `@SHELL_COMPLETIONS.md`

## Template Patterns

### Adding Configurable Options
1. Add prompt in `.chezmoi.yaml.tmpl` with `promptChoice`/`promptString`/`promptBool`
2. Store in `data:` section as quoted variable
3. Use in templates as `.variableName` (no `.data` prefix)
4. Pattern: check `hasKey`, prompt if missing, store result

### Chezmoi File Name Transformations

When working with chezmoi templates, reference files by their **FINAL deployed paths**, not source paths:

**Transformations**:
- `dot_` prefix → `.` (e.g., `dot_config` → `.config/`)
- `dot_foo` → `.foo` (e.g., `dot_bashrc` → `.bashrc`)
- `executable_` prefix → removed + `chmod +x`
- `.tmpl` extension → removed after template processing

**Examples**:
- Source: `dot_config/crush/CRUSH.md.tmpl` → Deployed: `~/.config/crush/CRUSH.md`
- Source: `CLAUDE.local.md.tmpl` → Deployed: `~/CLAUDE.local.md`
- Source: `dot_codex/AGENTS.md.tmpl` → Deployed: `~/.codex/AGENTS.md`

**Template References**: Always use source paths in `{{ template "..." . }}` directives (relative to `~/.local/share/chezmoi/`)
