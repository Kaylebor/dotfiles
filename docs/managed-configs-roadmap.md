# Managed Configurations System - Roadmap

This document outlines potential improvements and extensions for the managed configuration system.

## Completed Features ✅

### ✅ Backup Rotation (IMPLEMENTED)
**Goal**: Keep only the last 3-5 backups per configuration file

**Implementation**:
- `manage_backup_rotation()` function finds and removes excess backups
- `create_backup_with_rotation()` creates backup and manages rotation
- Configurable per-config (`max_backups`) with global default (5)
- Timestamp-based sorting with dry-run support

**Configuration**:
```yaml
global:
  max_backups: 5  # Global default

managed_configs:
  cursor_settings:
    max_backups: 3  # Per-config override
```

**Benefits**:
- ✅ Prevents backup accumulation
- ✅ Maintains safety with recent backups  
- ✅ Configurable limits per configuration
- ✅ Dry-run mode shows what would be removed

### ✅ Dry-Run Mode (IMPLEMENTED)
**Goal**: Show what would change without applying when `--dry-run` is passed

**Implementation**:
- Parses `CHEZMOI_ARGS` for `--dry-run`/`-n` and `--verbose`/`-v` flags
- Skips all file writes when in dry-run mode
- Shows proposed changes with diff output
- Supports verbose logging control
- Works with backup rotation (shows what backups would be created/removed)

```bash
# Usage examples:
chezmoi apply --dry-run          # Preview all changes
chezmoi apply --dry-run --verbose # Preview with detailed logging
```

**Features**:
- ✅ Skip file writes in dry-run mode
- ✅ Show all proposed changes with diffs
- ✅ Verbose output control
- ✅ Backup rotation preview

## Priority Improvements

### 5. JSONC/JSON5 Support
**Goal**: Properly handle JSON with comments without stripping them

**Dependencies**:
- Add `jq` alternatives that support JSONC/JSON5 to packages.yml
- Ensure tools are installed via run_onchange_before script
- Research Chezmoi script execution order (Context7)

**Tools to evaluate**:
- `json5` - Node.js based JSON5 parser
- `jsonc-parser` - Microsoft's JSONC parser
- `yq` - Already in our stack, check JSON5 support
- `dasel` - Multi-format data selector

**Implementation**:
- Detect file format (JSON vs JSONC vs JSON5)
- Use appropriate parser based on format
- Preserve comments and formatting
- Maintain compatibility with standard JSON

### 6. Config Validation Hooks
**Goal**: Run app-specific validation before applying changes

**Implementation**:
- Add optional `validation_command` field to metadata
- Run validation on merged result before applying
- Support for different validation strategies:
  - Exit code based (0 = valid)
  - Output parsing (look for "error" strings)
  - JSON schema validation

**Example validations**:
- Zed: `zed --validate-config` (if it exists)
- JSON schema validation with `ajv-cli`
- Custom validation scripts

### 7. Support for Alternative Formats
**Goal**: Extend system to handle YAML, TOML, INI files

**Format-specific tools**:
- **YAML**: `yq` for merging (already available)
- **TOML**: `tomlq` or `dasel`
- **INI**: `crudini` or custom parser

**Implementation approach**:
- Detect format from file extension or metadata
- Use format-appropriate merge tool
- Maintain same user experience across formats
- Consider format-specific merge strategies

**Priority formats**:
1. YAML - Many configs use it
2. TOML - Rust/modern tools
3. INI - Legacy but still common

### 8. Field Exclusion (Alternative to Preserve)
**Goal**: Explicitly exclude fields from templates rather than preserve

**Consideration**: Since templates are manually managed, this might not be as useful as initially thought. However, it could be helpful for:
- Removing sensitive fields automatically
- Cleaning up deprecated fields
- Ensuring certain fields never get templated

**Decision**: Keep as low priority, revisit if use cases emerge

### 10. Status Command
**Goal**: Create a status overview for managed configurations

**Features**:
- List all managed configurations
- Show last modified times
- Detect local changes vs template
- List available backups per config
- Show current merge strategy

**Implementation ideas**:
- Create a `chezmoi-managed-configs-status` script
- Parse metadata and check actual files
- Pretty-print table output
- Optional JSON output for scripting

## Research Questions (ANSWERED)

### 1. Chezmoi script execution order
**Answer**: Scripts execute in this order:
- `.chezmoiscripts/run_before_*` - Run before any files are updated
- File operations (copy, create, modify files)
- `.chezmoiscripts/run_after_*` - Run after all files are updated

**Modifiers**:
- `once_` - Runs only once ever (tracked in persistent state)
- `onchange_` - Runs when script content changes (hash-based)
- No modifier - Runs every time

**Naming convention**: `run_[once_|onchange_][before_|after_]name.sh[.tmpl]`

### 2. Chezmoi environment variables ✅ CONFIRMED
**Available variables**:
- `CHEZMOI_ARGS` - Full command line arguments
- `CHEZMOI_COMMAND` - Current command (apply, init, etc.)
- `CHEZMOI_OS`, `CHEZMOI_ARCH` - System info
- `CHEZMOI_SOURCE_DIR`, `CHEZMOI_DEST_DIR` - Directories
- `CHEZMOI_USERNAME`, `CHEZMOI_HOSTNAME` - User/system info
- Plus all template variables as `CHEZMOI_*` environment variables

### 3. Package availability timing
**Answer**: Packages are available after `run_onchange_before_install-packages.bash.tmpl` runs.
- Our `run_onchange_managed-configs.sh.tmpl` runs alphabetically after
- The `before_` prefix ensures packages install before file operations
- Safe to depend on packages in our merge script

## Next Steps

1. ~~Research Chezmoi flag/environment access~~ ✅ DONE
2. ~~Implement simple backup rotation (Quick win)~~ ✅ DONE
3. ~~Add dry-run mode support using `CHEZMOI_ARGS` detection~~ ✅ DONE
4. Investigate JSONC/JSON5 tools and add to packages
5. Prototype validation hooks with one config
6. Evaluate YAML/TOML support complexity
7. Create status command script

## Implementation Notes

### Backup Rotation Details
- Uses `find` with timestamp-based sorting to locate backup files
- Pattern matching: `${filename}.backup.*` 
- Preserves most recent N backups, removes older ones
- Integrates with dry-run mode for safe testing
- Per-config limits override global settings

### Dry-Run Mode Details
- Environment variable parsing: `CHEZMOI_ARGS` contains full command line
- Flag detection supports both long (`--dry-run`) and short (`-n`) forms
- All file operations are conditional on `$DRY_RUN` variable
- Diff output uses `difft` when available, falls back to `diff -u`
- Verbose logging controlled by `--verbose`/`-v` flag detection