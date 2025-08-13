# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Chezmoi-managed dotfiles for cross-platform development (macOS/Arch Linux) using Go templating.

## Key Commands

**Chezmoi**: `apply`, `diff`, `add <file>`, `edit <file>`, `cd`  
**Tools**: `mise install`, `bat cache --build`, `nvim --headless +PlugInstall +qall`  
**Auto-run scripts**: `run_once_*` (Homebrew install), `run_onchange_*` (package updates)

## Architecture

### File Structure
- `dot_*` → `.*` files in home
- `.tmpl` → Go template files
- `.chezmoidata/` → YAML package lists/URLs
- Platform detection via template conditionals

### Package Management
- **macOS**: Homebrew (`.chezmoidata/packages.yml`)
- **Arch**: paru AUR helper
- **Runtimes**: mise (formerly rtx)
- **Editors**: vim-plug (Neovim), elpaca (Emacs)

See @PACKAGE_MANAGEMENT.md for advanced configuration and Homebrew path migration

### Stack
**Languages**: Ruby, Node, Go, Python, Java (GraalVM), Elixir, Erlang, Rust, Deno, Bun  
**Editors**: Emacs (primary), Neovim, Helix, Zed  
**Shells**: Fish (primary), Zsh  
**Tools**: Git+delta, ripgrep, fd, bat, eza, starship, tmux, miller, 1Password SSH

### 1Password Integration
- SSH signing for git
- Template data via `onepasswordRead`
- Bypass and fallback options available

See @ONEPASSWORD_SETUP.md for detailed setup and troubleshooting

### Templates
- OS: `.chezmoi.os` ("darwin"/"linux")
- Linux distro: `.chezmoi.osRelease.idLike`
- Command check: `lookPath`
- Path check: `stat`

## Development Workflow

1. Edit in chezmoi source directory
2. Preview: `chezmoi diff`
3. Deploy: `chezmoi apply`
4. Add packages to `.chezmoidata/packages.yml`

## Fuzzy Finders

**Television** (primary): Modern channel-based finder with `tv init`/`tv update-channels`  
**fzf** (legacy): Kept for Neovim integration only

## Shell Completions

**Carapace**: Universal completion engine supporting Fish, Zsh, Bash, and more.

See @SHELL_COMPLETIONS.md for framework bridges, configuration, and usage details