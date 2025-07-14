# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a chezmoi-managed dotfiles repository for cross-platform development environment setup (macOS and Arch Linux). It uses Go templating to conditionally configure tools and packages based on the operating system.

## Key Commands

### Chezmoi Management
- `chezmoi apply` - Apply changes to the system
- `chezmoi diff` - See what changes would be applied
- `chezmoi add <file>` - Add a new file to chezmoi management
- `chezmoi edit <file>` - Edit a managed file
- `chezmoi cd` - Change to the chezmoi source directory

### Development Tools Setup
- `mise install` - Install all programming language runtimes and tools
- `bat cache --build` - Rebuild bat syntax highlighting cache (required after theme changes)
- `nvim --headless +PlugInstall +qall` - Install/update Neovim plugins

### System Package Installation
Scripts are automatically run by chezmoi:
- `run_once_0-install-brew.bash.tmpl` - Initial Homebrew installation (macOS only)
- `run_onchange_before_brew-install-packages.bash.tmpl` - Install packages via Homebrew/paru
- `run_onchange_98_tools_update.bash` - Update development tools
- `run_onchange_fish.bash.tmpl` - Fish shell configuration

## Architecture

### Configuration Structure
- `dot_*` files map to `.*` files in home directory
- `.tmpl` suffix indicates Go template files processed by chezmoi
- `.chezmoidata/` contains YAML data files for package lists and URLs
- Platform-specific configurations use chezmoi template conditionals

### Package Management
- **macOS**: Homebrew (brews and casks defined in `.chezmoidata/packages.yml`)
- **Arch Linux**: paru AUR helper (packages defined in `.chezmoidata/packages.yml`)
- **Development tools**: mise (formerly rtx) manages language runtimes
- **Editor plugins**: Neovim via vim-plug, Emacs via elpaca

#### Homebrew Package Configuration
The `packages.yml` file supports advanced package configurations:
- **Global configurations**: Apply to all Homebrew installation types
- **Alternative-specific configurations**: Only applied when using alternative Homebrew prefix (`~/.homebrew`)
- **Environment variables**: Set per-package for complex builds (e.g., gcc with header padding flags)
- **Build arguments**: Custom compilation flags and options

Example package with alternative-specific configuration:
```yaml
- name: "gcc"
  alternative_only:
    args:
      - "build-from-source"
    env:
      LDFLAGS: "-Wl,-headerpad_max_install_names"
      BOOT_LDFLAGS: "-Wl,-headerpad_max_install_names"
      CFLAGS_FOR_TARGET: "-Wl,-headerpad_max_install_names"
      CXXFLAGS_FOR_TARGET: "-Wl,-headerpad_max_install_names"
      HOMEBREW_NO_INSTALL_CLEANUP: "1"
```

#### Known Limitations
- **GCC with Alternative Homebrew**: May show dylib fixing errors during post-install but compiles and runs correctly
- **Alternative installation**: All packages built from source, significantly longer installation times

### Key Tools and Languages
Development environment includes:
- **Languages**: Ruby, Node.js, Go, Python, Java (GraalVM), Elixir, Erlang, Rust, Deno, Bun
- **Editors**: Emacs (primary), Neovim, Helix, Zed
- **Shells**: Fish (primary), Zsh
- **Version control**: Git with delta diff viewer, difftastic, 1Password SSH signing
- **CLI tools**: ripgrep, fd, fzf, bat, eza, starship prompt, tmux

### Authentication
Uses 1Password for:
- SSH key signing (configured in git)
- Secure storage of signing keys and tokens
- Template data injection via `onepasswordRead` function

#### 1Password CLI Bypass
For environments where 1Password CLI is broken (e.g., MDM setups), the repository includes bypass mechanisms:
- **Configuration flag**: Set `skip1Password: true` in chezmoi config or use `CHEZMOI_SKIP_1PASSWORD=true`
- **Git signing**: Automatically disables SSH signing when 1Password is skipped (prevents commit failures)
- **Environment variable fallbacks**:
  - `CHEZMOI_GIT_SIGNING_KEY` - SSH signing key for git commits (used in gitconfig but signing disabled when skipped)
  - `CHEZMOI_GEMINI_API_KEY` - Gemini API key for aider
  - `CHEZMOI_OPENROUTER_API_KEY` - OpenRouter API key for aider
  - `CHEZMOI_DEEPSEEK_API_KEY` - DeepSeek API key for aider

### Templating System
chezmoi templates use:
- `.chezmoi.os` for OS detection ("darwin" or "linux")
- `.chezmoi.osRelease.idLike` for Linux distribution detection
- `lookPath` function to check if commands exist
- `stat` function to check if paths exist
- Package lists from `.chezmoidata/packages.yml`

## Development Workflow

When modifying this repository:
1. Edit files in the chezmoi source directory (this directory)
2. Test changes with `chezmoi diff` before applying
3. Use `chezmoi apply` to deploy changes to the system
4. For new packages, add them to the appropriate section in `.chezmoidata/packages.yml`
5. Template files should handle both macOS and Linux configurations where applicable