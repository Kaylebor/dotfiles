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

### Advanced Homebrew Config
Supports per-package build flags and environment vars:
```yaml
- name: "gcc"
  alternative_only:
    args: ["build-from-source"]
    env:
      LDFLAGS: "-Wl,-headerpad_max_install_names"
```

### Stack
**Languages**: Ruby, Node, Go, Python, Java (GraalVM), Elixir, Erlang, Rust, Deno, Bun  
**Editors**: Emacs (primary), Neovim, Helix, Zed  
**Shells**: Fish (primary), Zsh  
**Tools**: Git+delta, ripgrep, fd, bat, eza, starship, tmux, 1Password SSH

### 1Password Integration
- SSH signing for git
- Template data via `onepasswordRead`
- **Bypass**: Set `skip1Password: true` or `CHEZMOI_SKIP_1PASSWORD=true`
- **Fallback env vars**: `CHEZMOI_GIT_SIGNING_KEY`, `CHEZMOI_*_API_KEY`

### Homebrew Path Migration
Auto-detects path changes (`~/.homebrew` → `~/homebrew`):
1. Tracks state in `~/.config/chezmoi/.homebrew-state`
2. Identifies packages with embedded paths (gcc, llvm, binutils, etc.)
3. Auto-rebuilds with `CHEZMOI_FORCE_REINSTALL_PACKAGES`

Manual fix:
```bash
CHEZMOI_FORCE_REINSTALL_PACKAGES="gcc,llvm" chezmoi apply
```

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