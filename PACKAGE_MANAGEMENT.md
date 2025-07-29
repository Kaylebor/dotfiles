# Package Management Details

## Architecture

### Package Lists
Package definitions are stored in `.chezmoidata/packages.yml` with platform-specific installation.

### macOS (Homebrew)
Primary package manager with custom installation logic supporting:
- Standard packages
- Cask applications
- Build-from-source options
- Custom environment variables

### Linux (Arch)
Uses paru AUR helper for package installation.

### Runtime Management
Uses mise (formerly rtx) for language runtimes:
- Ruby, Node, Go, Python
- Java (GraalVM), Elixir, Erlang
- Rust, Deno, Bun

## Advanced Homebrew Configuration

### Per-Package Build Flags
Supports custom build arguments and environment variables:

```yaml
- name: "gcc"
  alternative_only:
    args: ["build-from-source"]
    env:
      LDFLAGS: "-Wl,-headerpad_max_install_names"
      HOMEBREW_MAKE_JOBS: "8"
```

### Homebrew Path Migration

Automatically detects and handles Homebrew path changes (e.g., `~/.homebrew` → `~/homebrew`):

1. **State Tracking**: Maintains migration state in `~/.config/chezmoi/.homebrew-state`
2. **Path Detection**: Identifies packages with embedded paths:
   - gcc, llvm, binutils
   - gfortran, openblas
   - node@20, ruby
   - postgresql@16, mysql

3. **Auto-Rebuild**: Packages with embedded paths are automatically rebuilt when paths change

### Manual Intervention

Force reinstall specific packages:
```bash
CHEZMOI_FORCE_REINSTALL_PACKAGES="gcc,llvm" chezmoi apply
```

Force reinstall all affected packages:
```bash
CHEZMOI_FORCE_REINSTALL_ALL=true chezmoi apply
```

## Editor Package Management

### Neovim
Uses vim-plug for plugin management:
```bash
nvim --headless +PlugInstall +qall
```

### Emacs
Uses elpaca as the package manager with automatic installation.