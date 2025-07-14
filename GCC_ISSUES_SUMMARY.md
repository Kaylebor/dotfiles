# GCC Issues Summary - Alternative Homebrew Installation

## Problem Overview

When using alternative Homebrew installation (`~/.homebrew`) with gcc built from source, we encounter several issues:

1. **Primary Issue**: GCC shows dylib fixing errors during installation but works correctly
2. **Secondary Issue**: Other packages (enchant/aspell, emacs) fail to compile due to incorrect compiler detection
3. **Root Cause**: System picks up Apple's clang (`/usr/bin/gcc`) instead of Homebrew's gcc (`gcc-15`)

## Current GCC Configuration

### Homebrew Package Configuration
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

### Key Observations
- GCC installs as `gcc-15` (version-specific binary)
- No unversioned `gcc` symlink (intentional by Homebrew)
- Build systems default to `/usr/bin/gcc` (Apple clang)
- Dylib errors are cosmetic - gcc functions correctly

## Attempted Solutions

### 1. Patch System (Abandoned)
- **Attempted**: Generic patch system for Homebrew formulas
- **Issues**: 
  - Homebrew doesn't support `file://` URLs
  - Complex inline DATA patches failed
  - chezmoi format errors with `.patch` files
- **Result**: Removed entirely per user request

### 2. Environment Variable Isolation
- **Implemented**: Subshell isolation for package-specific environment variables
- **Purpose**: Prevent variable leakage between packages
- **Status**: Working correctly

### 3. 1Password CLI Bypass
- **Implemented**: Skip 1Password when CLI is broken (MDM environments)
- **Features**: 
  - Environment variable fallbacks
  - Automatic git signing disable
  - Configuration flag support
- **Status**: Working correctly

## Current Solution (Latest)

### Environment Variables Setup
Added to all shell configurations using template logic:

```bash
# Library paths
export LIBRARY_PATH="$HOME/.homebrew/opt/gcc/lib/gcc/current:$LIBRARY_PATH"

# C/C++ include paths  
export C_INCLUDE_PATH="$HOME/.homebrew/opt/gcc/include:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="$HOME/.homebrew/opt/gcc/include/c++/15:$CPLUS_INCLUDE_PATH"

# Set CC and CXX to use the Homebrew gcc
export CC=gcc-15
export CXX=g++-15
```

### Files Modified
- `.fish_env.fish` - Fish shell environment setup
- `dot_profile.tmpl` - POSIX shell profile
- `dot_zprofile.tmpl` - Zsh profile
- `dot_bashrc.tmpl` - Bash interactive shell
- `dot_zshrc.tmpl` - Zsh interactive shell

## Expected Outcome

With these environment variables set:
1. **CC/CXX**: Build systems will use `gcc-15` instead of system clang
2. **LIBRARY_PATH**: Linker will find gcc runtime libraries
3. **C_INCLUDE_PATH**: C compiler will find gcc headers
4. **CPLUS_INCLUDE_PATH**: C++ compiler will find gcc headers

This should resolve compilation issues for packages like enchant/aspell and emacs.

## Testing Required

After restart and `chezmoi apply`:
1. Verify environment variables are set: `echo $CC $CXX $LIBRARY_PATH`
2. Test gcc detection: `which gcc` vs `which gcc-15`
3. Attempt to install enchant/aspell
4. Test emacs compilation with gcc

## Notes

- GCC dylib errors are cosmetic and don't affect functionality
- Template logic ensures settings only apply to alternative Homebrew installations
- Environment variables are set at shell initialization time
- No symlinks created to avoid system integrity issues