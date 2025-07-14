# MDM and ARM Mac Development Environment Fixes

This document outlines solutions for common development issues encountered on ARM-based Macs with MDM (Mobile Device Management) restrictions, particularly when using alternative Homebrew installations.

## Overview

MDM-managed Macs often restrict access to standard system locations like `/opt/homebrew`, requiring alternative Homebrew installations (typically at `~/.homebrew`). This creates several compatibility issues with development tools and language runtimes.

## Key Issues and Solutions

### 0. SentinelOne Endpoint Security Interference (Critical)

**Problem**: SentinelOne endpoint security software aggressively quarantines development tools, causing:
- `brew` binary disappearing after running `brew services start`
- Ruby flagged as "persistence_deception" threat
- Development tools randomly quarantined or blocked
- Build processes failing due to missing executables

**Symptoms**:
- `brew not found` after previously working
- Ruby/Node/other language runtimes disappearing
- Services failing to start
- Executables in `~/.homebrew/bin/` or mise installations being quarantined

**Solutions**:
1. **Request IT to add exclusions** for development directories:
   ```
   ~/.homebrew/
   ~/.local/share/mise/
   ~/.cargo/
   ~/work/
   ```

2. **Monitor SentinelOne threat history** for false positives and request whitelisting

3. **Use system-installed tools** where possible (less likely to be flagged)

4. **Backup and restore strategy**: Keep scripts to quickly reinstall tools when quarantined

5. **Alternative installation methods**: Consider using system package managers or Docker when possible

**Note**: This is often the root cause of "mysterious" development environment failures on MDM-managed machines.

## Key Issues and Solutions

### 1. Ruby Native Extension Compilation Issues

**Problem**: Ruby gems with native extensions (like RMagick) fail to compile due to:
- Ruby compiled with GCC but system expects clang
- Missing or incorrect C++ compiler configuration
- ImageMagick version compatibility issues

**Solution**: Force Ruby to use system clang compilers and ImageMagick 6

```toml
# In mise/config.toml (or equivalent tool configuration)
CC="/usr/bin/clang"
CXX="/usr/bin/clang++"
USE_IMAGEMAGICK_6="1"
```

**Package Changes**:
```yaml
# Use ImageMagick 6 instead of 7 for better gem compatibility
- "imagemagick@6"  # instead of "imagemagick"
```

**Critical Step**: After setting environment variables, rebuild Ruby:
```bash
mise uninstall ruby@3.2.2
mise install ruby@3.2.2
```

### 2. Homebrew Formula Patching System

**Problem**: Some Homebrew formulas fail to compile with alternative installations due to hardcoded paths or compiler assumptions.

**Solution**: Implement a generalized formula patching system that:
- Detects when formulas need patches for alternative Homebrew
- Applies patches automatically during installation
- Validates patch integrity with checksums

**Key Components**:
- `.chezmoidata/packages.yml` - Configuration for patches and expected hashes
- `run_onchange_before_brew-install-packages.bash.tmpl` - Automatic patch application
- `scripts/patches/` - Directory containing formula-specific patches

### 3. GCC Build Issues

**Problem**: GCC compilation from source fails with header padding issues on ARM Macs.

**Solution**: Set specific build flags for GCC when using alternative Homebrew:

```yaml
# In packages.yml
gcc:
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

### 4. 1Password CLI Integration Issues

**Problem**: 1Password CLI may be broken or restricted in MDM environments.

**Solution**: Implement bypass mechanisms with fallback environment variables:

```bash
# Environment variables for 1Password bypass
CHEZMOI_SKIP_1PASSWORD=true
CHEZMOI_GIT_SIGNING_KEY="ssh-ed25519 AAAAC3..."
CHEZMOI_GEMINI_API_KEY="..."
CHEZMOI_OPENROUTER_API_KEY="..."
CHEZMOI_DEEPSEEK_API_KEY="..."
```

**Template Logic**:
```go
{{- $skip1Password := or (env "CHEZMOI_SKIP_1PASSWORD") (.chezmoi.config.data.skip1Password) -}}
{{- if not $skip1Password -}}
    {{- /* Use 1Password CLI */ -}}
{{- else -}}
    {{- /* Use environment variable fallbacks */ -}}
{{- end -}}
```

### 5. Service Installation Issues

**Problem**: System services (like Redis) may have broken installations or missing binaries.

**Solution**: 
- Use `brew services` to manage services
- Reinstall packages if binaries are missing
- Check service status with `brew services list`

```bash
# Fix broken Redis installation
brew services stop redis
brew reinstall redis
brew services start redis
```

### 6. File Descriptor Limits (Too Many Open Files)

**Problem**: Applications like Sidekiq fail with "too many open files" errors due to restrictive system limits common on MDM-managed Macs.

**Symptoms**:
- `Errno::EMFILE: Too many open files` errors
- Services failing to start connections
- Database connection failures
- Low system maxfiles limit (256) vs high application needs

**Root Cause**: MDM environments often have very restrictive file descriptor limits that prevent development applications from opening sufficient connections.

**Solution**: Increase file descriptor limits using appropriate method based on user privileges:

1. **For admin users**:
```bash
sudo launchctl limit maxfiles 65536 200000
```

2. **For non-admin users** (common in MDM): Create user-level LaunchAgent
3. **Shell-level fallback**: Add `ulimit -n 65536` to shell profiles

**Implementation**: The solution automatically detects admin privileges and uses the appropriate method.

### 7. PKG_CONFIG_PATH Configuration

**Problem**: Build tools can't find libraries installed with alternative Homebrew.

**Solution**: Ensure PKG_CONFIG_PATH includes alternative Homebrew paths:

```toml
PKG_CONFIG_PATH="/alternative/homebrew/opt/imagemagick@6/lib/pkgconfig:/alternative/homebrew/opt/openblas/lib/pkgconfig:..."
```

## Architecture-Specific Considerations

### ARM64 vs x86_64 Compatibility
- Some packages may need architecture-specific handling
- Use `arch` command to check current architecture
- Consider Rosetta 2 implications for x86_64 dependencies

### Compiler Toolchain Issues
- ARM Macs use different default compiler paths
- Ensure consistent compiler usage across all build tools
- Set both `CC` and `CXX` explicitly to avoid mixed toolchains

## Implementation Pattern

1. **Detect Environment**: Use chezmoi templating to detect alternative Homebrew
2. **Scope Changes**: Apply fixes only to alternative installations
3. **Environment Variables**: Set compiler and build flags globally
4. **Package Management**: Use version-specific packages where needed
5. **Rebuild Dependencies**: Reinstall language runtimes after environment changes

## Testing Strategy

1. **Clean Environment**: Test with fresh language runtime installations
2. **Native Extension Compilation**: Verify gems with C/C++ extensions compile
3. **Service Management**: Ensure system services start properly
4. **Package Compatibility**: Test common development packages

## Example Complete Configuration

```toml
# mise/config.toml.tmpl
{{- if eq .homebrewInstallType "alternative" }}
## Compiler configuration for alternative Homebrew
CC="/usr/bin/clang"
CXX="/usr/bin/clang++"
USE_IMAGEMAGICK_6="1"
CFLAGS="-Wno-error=implicit-function-declaration"

## PKG_CONFIG_PATH with alternative Homebrew packages
PKG_CONFIG_PATH="~/.homebrew/opt/imagemagick@6/lib/pkgconfig:~/.homebrew/opt/openblas/lib/pkgconfig:..."
{{- end }}
```

This comprehensive approach ensures a robust development environment on MDM-managed ARM Macs while maintaining compatibility with standard installations.