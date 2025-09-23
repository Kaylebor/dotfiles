# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Security Guidelines

- **Never display password or secret values** - When testing auth-source or password retrieval, check for existence/length only, not the actual value
- **Avoid exposing sensitive data** - Use methods like `(if password "<password exists>" "<no password>")` instead of displaying values

## Repository Overview

Chezmoi-managed dotfiles for cross-platform development (macOS/Arch Linux) using Go templating.

## Key Commands

**Chezmoi**: `apply`, `diff`, `add <file>`, `edit <file>`, `cd`  
**Tools**: `mise install`, `bat cache --build`, `nvim --headless +PlugInstall +qall`  
**Auto-run scripts**: `run_once_*` (Homebrew install), `run_onchange_*` (package updates)

Note: `chezmoi apply` works from any directory - it automatically uses `~/.local/share/chezmoi` as source and `$HOME` as target.

**Important**: If `chezmoi apply` fails with 1Password errors related to TTY availability, Claude Code cannot run it directly. In this case, ask the user to run the command manually in their terminal.

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
**Editors**: Emacs (primary), Neovim, Zed  
**Shells**: Fish (primary), Zsh  
**Tools**: Git+difftastic, ripgrep, fd, bat, eza, tmux, miller, 1Password SSH

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

**Important**: Always edit chezmoi source files (e.g., `dot_gitignore`), not the generated target files (e.g., `~/.gitignore`). Use `chezmoi apply` to deploy changes.

## Fuzzy Finders

**Television** (primary): Modern channel-based finder with `tv init`/`tv update-channels`  
**fzf** (legacy): Kept for Neovim integration only

## Shell Completions

**Carapace**: Universal completion engine supporting Fish, Zsh, Bash, and more.

See @SHELL_COMPLETIONS.md for framework bridges, configuration, and usage details

## Chezmoi Documentation
Context7 Library ID: /twpayne/chezmoi

### Adding Configurable Options
1. Add prompt in `.chezmoi.yaml.tmpl` with `promptChoice`/`promptString`/`promptBool`
2. Store in `data:` section as quoted variable
3. Use in templates as `.variableName` (no `.data` prefix)
4. Pattern: check `hasKey`, prompt if missing, store result
