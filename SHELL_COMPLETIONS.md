# Shell Completions with Carapace

## Overview

Carapace is a universal completion engine that provides consistent completions across Fish, Zsh, Bash, and other shells.

## Completion Strategy

### 1. Framework Bridges (Preferred)
Direct communication with tool's built-in completion system:

#### Cobra (Go)
- `gh`, `kubectl`, `docker`, `helm`, `hugo`
- `chezmoi`, `golangci-lint`, `kind`

#### Clap (Rust)
- `fd`, `rg`, `bat`, `eza`, `delta`
- `mise`, `just`, `starship`

#### Click (Python)
- `pgcli`, `litecli`
- Most Python CLI tools

### 2. Shell Bridges (Fallback)
Parse shell-specific completions when framework bridges aren't available.
Priority order: zsh → fish → bash

## Configuration

### Bridge Configuration
Located in `.config/carapace/bridges.yaml`:
```yaml
# Example: Force a tool to use a specific framework
gh: cobra
docker: cobra
npm: zsh  # Use zsh completions for npm
```

### Custom Specifications
For tools without framework support, create specs in `.config/carapace/specs/`:
- `aider.yaml` - AI coding assistant
- `jest.yaml` - JavaScript testing framework

## Usage

### Detect Tool Framework
```bash
carapace --detect <tool>
```
Shows which completion framework a tool uses.

### List Available Completions
```bash
carapace --list
```

### Update Completions
Framework completions update automatically when tools are updated.
No manual regeneration needed.

## Benefits

1. **Consistency**: Same completion behavior across all shells
2. **Auto-updates**: Framework completions stay in sync with tools
3. **Performance**: Faster than shell-specific completion scripts
4. **Universal**: Works with any shell that carapace supports