# MDM and ARM Mac Development Environment Fixes

This document outlines solutions for common development issues encountered on ARM-based Macs with MDM (Mobile Device Management) restrictions, particularly when using alternative Homebrew installations.

## Overview

MDM-managed Macs often restrict access to standard system locations like `/opt/homebrew`, requiring alternative Homebrew installations (typically at `~/homebrew`). This creates several compatibility issues with development tools and language runtimes.

## Key Issues and Solutions

### 0. SentinelOne Endpoint Security Interference (Critical)

**Problem**: SentinelOne endpoint security software aggressively quarantines development tools, causing:

- `brew` binary disappearing after running `brew services start`
- Ruby flagged as "persistence_deception" threat
- Development tools randomly quarantined or blocked
- Build processes failing due to missing executables

**Symptoms**:

- `brew not found` after previously working
- Ruby/Node/Go/other language runtimes disappearing
- Services failing to start
- Executables in `$(brew --prefix)/bin/` or language manager installations being quarantined

**Solutions**:

1. **Request IT to add exclusions** for development directories:

   ```
   $(brew --prefix)/
   ~/.local/share/mise/
   ~/.local/share/asdf/
   ~/.rbenv/
   ~/.rvm/
   ~/.cargo/
   ~/work/
   ```

2. **Monitor SentinelOne threat history** for false positives and request whitelisting

3. **Use system-installed tools** where possible (less likely to be flagged)

4. **Backup and restore strategy**: Keep scripts to quickly reinstall tools when quarantined

5. **Alternative installation methods**: Consider using system package managers or Docker when possible

**Note**: This is often the root cause of "mysterious" development environment failures on MDM-managed machines.

### 1. Ruby Native Extension Compilation Issues

**Problem**: Ruby gems with native extensions (like RMagick) fail to compile due to:

- Ruby compiled with GCC but system expects clang
- Missing or incorrect C++ compiler configuration
- ImageMagick version compatibility issues

**Solution**: Force Ruby to use system clang compilers instead of GCC

#### For [rbenv](https://github.com/rbenv/rbenv) / [rvm](https://rvm.io) / [asdf](https://asdf-vm.com) users:

```bash
# Add to ~/.zshrc (or ~/.bashrc, ~/.profile)
export CC="/usr/bin/clang"
export CXX="/usr/bin/clang++"
export CPLUS_INCLUDE_PATH="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"
export C_INCLUDE_PATH="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
export USE_IMAGEMAGICK_6="1"
export CFLAGS="-Wno-error=implicit-function-declaration"
```

#### For [mise](https://mise.jdx.dev) users:

```toml
# In ~/.config/mise/config.toml (or project-specific .mise.toml)
[env]
CC="/usr/bin/clang"
CXX="/usr/bin/clang++"
CPLUS_INCLUDE_PATH="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"
C_INCLUDE_PATH="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
```

**Note**: [mise](https://mise.jdx.dev) allows project-specific configurations. You can create a `.mise.toml` file in your project root to apply these settings only to that project, or use the global `~/.config/mise/config.toml` for system-wide settings. See the [mise configuration documentation](https://mise.jdx.dev/configuration.html) for more details.

**Package Changes**:
Install ImageMagick 6 instead of 7 for better gem compatibility:

```bash
brew install imagemagick@6
```

**Environment Variable**: Set `USE_IMAGEMAGICK_6="1"` to force gems to use ImageMagick 6 even when ImageMagick 7 is also installed.

**Critical Step**: After setting environment variables, rebuild Ruby:

```bash
# For rbenv
rbenv uninstall 3.2.2
rbenv install 3.2.2

# For rvm
rvm uninstall 3.2.2
rvm install 3.2.2

# For asdf
asdf uninstall ruby 3.2.2
asdf install ruby 3.2.2

# For mise
mise uninstall ruby@3.2.2
mise install ruby@3.2.2
```

### 2. GCC Build Issues

**Problem**: GCC compilation from source fails with header padding issues on ARM Macs.

**Solution**: When installing GCC with alternative Homebrew, use specific build flags:

```bash
# Set environment variables before installing GCC
export LDFLAGS="-Wl,-headerpad_max_install_names"
export BOOT_LDFLAGS="-Wl,-headerpad_max_install_names"
export CFLAGS_FOR_TARGET="-Wl,-headerpad_max_install_names"
export CXXFLAGS_FOR_TARGET="-Wl,-headerpad_max_install_names"
export HOMEBREW_NO_INSTALL_CLEANUP="1"

# Install GCC from source
brew install gcc --build-from-source
```

**For [mise](https://mise.jdx.dev) users**: These environment variables can also be added to your mise configuration in `~/.config/mise/config.toml` under the `[env]` section.

### 4. Service Installation Issues

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

### 5. File Descriptor Limits (Too Many Open Files)

**Problem**: Applications like Sidekiq fail with "too many open files" errors due to restrictive system limits common on MDM-managed Macs.

**Symptoms**:

- `Errno::EMFILE: Too many open files` errors
- Services failing to start connections
- Database connection failures
- Low system maxfiles limit (256) vs high application needs

**Root Cause**: MDM environments often have very restrictive file descriptor limits that prevent development applications from opening sufficient connections.

**Solution**: Increase file descriptor limits:

#### For admin users:

```bash
sudo launchctl limit maxfiles 65536 200000
```

#### For non-admin users (common in MDM):

Create a LaunchAgent file at `~/Library/LaunchAgents/limit.maxfiles.plist`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>
    <key>Label</key>
    <string>limit.maxfiles</string>
    <key>ProgramArguments</key>
    <array>
      <string>launchctl</string>
      <string>limit</string>
      <string>maxfiles</string>
      <string>65536</string>
      <string>200000</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
    <key>ServiceIPC</key>
    <false/>
  </dict>
</plist>
```

Then load it:

```bash
launchctl load ~/Library/LaunchAgents/limit.maxfiles.plist
```

#### Shell-level fallback:

Add to your shell profile (~/.zshrc):

```bash
ulimit -n 65536
```

### 6. PKG_CONFIG_PATH Configuration

**Problem**: Build tools can't find libraries installed with alternative Homebrew.

**Solution**: Add to your shell profile (~/.zshrc):

```bash
export PKG_CONFIG_PATH="$(brew --prefix)/opt/imagemagick@6/lib/pkgconfig:$(brew --prefix)/opt/openblas/lib/pkgconfig:$(brew --prefix)/opt/lapack/lib/pkgconfig:$(brew --prefix)/opt/libpq/lib/pkgconfig:$(brew --prefix)/lib/pkgconfig"
export LDFLAGS="-L$(brew --prefix)/opt/llvm/lib -L$(brew --prefix)/opt/openblas/lib -L$(brew --prefix)/opt/lapack/lib -L$(brew --prefix)/opt/libpq/lib"
export LAPACK="$(ls $(brew --prefix)/opt/lapack/lib/liblapack*.dylib | head -1)"
export BLAS="$(ls $(brew --prefix)/opt/openblas/lib/libopenblasp-r*.dylib | head -1)"
```

**Troubleshooting**: If you encounter "package not found" errors for other libraries:

1. **Find the library location**:

   ```bash
   # Find where Homebrew installed the package
   brew list <package-name> | grep pkgconfig

   # Or search for .pc files
   find $(brew --prefix) -name "*.pc" -path "*/lib/pkgconfig/*" | grep <library-name>
   ```

2. **Add the path to PKG_CONFIG_PATH**:

   ```bash
   export PKG_CONFIG_PATH="$(brew --prefix)/opt/<package-name>/lib/pkgconfig:$PKG_CONFIG_PATH"
   ```

3. **Verify the configuration**:
   ```bash
   pkg-config --list-all | grep <library-name>
   ```

**Advanced: Dynamic Configuration with [mise](https://mise.jdx.dev) templating**:

For [mise](https://mise.jdx.dev) users, you can use templating to dynamically generate these paths:

```toml
# In ~/.config/mise/config.toml or .mise.toml
{% set brew_prefix = exec(command='brew --prefix') %}
[env]
LDFLAGS = "{{ get_env(name='LDFLAGS', default='') }} -L{{ brew_prefix }}/opt/llvm/lib -L{{ brew_prefix }}/opt/openblas/lib -L{{ brew_prefix }}/opt/lapack/lib -L{{ brew_prefix }}/opt/libpq/lib"
PKG_CONFIG_PATH = "{{ get_env(name='PKG_CONFIG_PATH', default='') }}:{{ brew_prefix }}/opt/imagemagick@6/lib/pkgconfig:{{ brew_prefix }}/opt/openblas/lib/pkgconfig:{{ brew_prefix }}/opt/lapack/lib/pkgconfig:{{ brew_prefix }}/opt/libpq/lib/pkgconfig:{{ brew_prefix }}/lib/pkgconfig"
LAPACK = "{{ exec(command='ls ' + brew_prefix + '/opt/lapack/lib/liblapack*.dylib | head -1') }}"
BLAS = "{{ exec(command='ls ' + brew_prefix + '/opt/openblas/lib/libopenblasp-r*.dylib | head -1') }}"
```

This approach uses `{% set %}` to store the Homebrew prefix once, then reuses it throughout the configuration. The `exec()` function runs shell commands and returns their output. See the [mise templating documentation](https://mise.jdx.dev/templates.html) for more details.

### 7. Advanced: Homebrew Formula Patching

**Problem**: Some Homebrew formulas fail to compile with alternative installations due to hardcoded paths or compiler assumptions.

**Solution**: For complex environments, you may need to implement formula patching. This involves:

- Automatically detecting when formulas need patches
- Applying patches during installation
- Validating patch integrity

**Additional Build Troubleshooting**:

1. **For packages that fail with parallel builds**:

   ```bash
   export HOMEBREW_MAKE_JOBS="1"
   brew install <package-name>
   ```

2. **For custom formula modifications**:
   ```bash
   export HOMEBREW_NO_INSTALL_FROM_API=1
   brew install <package-name>
   ```

**Resources**:

- [Homebrew Formula Cookbook](https://docs.brew.sh/Formula-Cookbook) - Official guide to creating and modifying formulas
- [Homebrew Patches](https://docs.brew.sh/Formula-Cookbook#patches) - How to apply patches to formulas
- [Custom Homebrew Taps](https://docs.brew.sh/How-to-Create-and-Maintain-a-Tap) - Creating your own formula repository

**Note**: This is an advanced solution typically needed only in highly restricted environments. Most issues can be resolved with the environment variable fixes above.

## Architecture-Specific Considerations

### ARM64 vs x86_64 Compatibility

- Some packages may need architecture-specific handling
- Use `arch` command to check current architecture
- Consider Rosetta 2 implications for x86_64 dependencies

### Compiler Toolchain Issues

- ARM Macs use different default compiler paths
- Ensure consistent compiler usage across all build tools
- Set both `CC` and `CXX` explicitly to avoid mixed toolchains

**Important**: There may be conflicts between system-wide compiler settings and project-specific ones:

- Some tools expect **system clang** (`/usr/bin/clang`) for compatibility
- Other tools may need **Homebrew GCC** for specific features
- Use project-specific tool configuration (like [mise](https://mise.jdx.dev)) to override global shell settings
- The [mise](https://mise.jdx.dev) configuration above will take precedence over global `CC`/`CXX` exports in shell profiles

## Shell Configuration Summary

Add these lines to your `~/.zshrc` (or `~/.bashrc` if using bash):

```bash
# Compiler configuration for ARM Mac development
export CC="/usr/bin/clang"
export CXX="/usr/bin/clang++"
export CPLUS_INCLUDE_PATH="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"
export C_INCLUDE_PATH="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
export USE_IMAGEMAGICK_6="1"
export CFLAGS="-Wno-error=implicit-function-declaration"


# PKG_CONFIG_PATH for alternative Homebrew
export PKG_CONFIG_PATH="$(brew --prefix)/opt/imagemagick@6/lib/pkgconfig:$(brew --prefix)/opt/openblas/lib/pkgconfig:$(brew --prefix)/opt/lapack/lib/pkgconfig:$(brew --prefix)/opt/libpq/lib/pkgconfig:$(brew --prefix)/lib/pkgconfig"
export LDFLAGS="-L$(brew --prefix)/opt/llvm/lib -L$(brew --prefix)/opt/openblas/lib -L$(brew --prefix)/opt/lapack/lib -L$(brew --prefix)/opt/libpq/lib"
export LAPACK="$(ls $(brew --prefix)/opt/lapack/lib/liblapack*.dylib | head -1)"
export BLAS="$(ls $(brew --prefix)/opt/openblas/lib/libopenblasp-r*.dylib | head -1)"


# File descriptor limits
ulimit -n 65536

# Ruby gem compilation
export USE_IMAGEMAGICK_6="1"
export CFLAGS="-Wno-error=implicit-function-declaration"
```
