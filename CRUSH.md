# CRUSH.md - Crush-Specific Enhancements

This file supplements `AGENTS.md` with Crush CLI-specific configurations and optimizations for this chezmoi repository.

## Crush Configuration

**Context Files Loaded** (via `dot_config/crush/crush.json`):
- `~/.config/crush/CRUSH.md` - Global Crush preferences for all repos
- `AGENTS.md` - Universal repo fundamentals (read by Crush by default)
- `CRUSH.md` - This file (repo-specific Crush enhancements)
- `CRUSH_INDEX.md` - Task tracking index

**System Prompt Configuration**:
- `system_prompt_prefix` - Prepended to every model invocation (set in `dot_config/crush/crush.json`)
- Used to enforce critical rules that degrade with long context windows
- Example: "🔴 CRITICAL: You MUST close ALL thinking sections before responding"

The system prompt prefix is particularly important for thinking models like Kimi K2 Thinking, where system prompts can get pushed out after many tool calls. This configuration ensures critical communication rules stay near the model's immediate context.

## Crush-Specific Workflows

### Preferred Tool Usage
- Use `agent` tool for complex research/implementation tasks
- Use `apply_patch` for surgical file edits
- Use `chezmoi execute-template` before applying to verify templates
- Always read files before editing (respects "ALWAYS READ BEFORE EDITING" rule)

### Cross-Platform Testing
When modifying OS-specific templates:
```bash
# Test macOS rendering
chezmoi execute-template --init --var "chezmoi.os=darwin" <file>

# Test Linux rendering  
chezmoi execute-template --init --var "chezmoi.os=linux" <file>
```

## Advanced Chezmoi Patterns

### Template Debugging
```bash
# Inspect all available variables
chezmoi data | jq .

# Check specific variable
chezmoi data | jq '.chezmoi.os'

# Render with custom variables
chezmoi execute-template --var "custom=value" file.tmpl
```

### Package Management Tricks
```bash
# Quick reinstall (preserves install options)
CHEZMOI_REINSTALL_PACKAGES="pkg1,pkg2" chezmoi apply

# Clean reinstall (removes options)
CHEZMOI_FORCE_REINSTALL_PACKAGES="pkg1" chezmoi apply

# Rebuild all outdated packages after Homebrew path migration
CHEZMOI_FORCE_REINSTALL_PACKAGES="$(chezmoi execute-template < '.chezmoidata/packages.yml' | yq '.darwin.brews | join(",")')" chezmoi apply
```

## Repository-Specific Notes

### Large Files
- `dot_local/bin/chezmoi-homebrew-manager` - Auto-generated, replace wholesale if updating
- `.chezmoidata/packages.yml` - Hand-edited, use apply_patch for changes
- Template files (`.tmpl`) - Use apply_patch, verify with execute-template

### Complex Templates
- `.chezmoi.yaml.tmpl` - Central configuration with extensive logic
- `dot_config/fish/config.fish.tmpl` - Complex shell configuration
- `run_once_*.tmpl` - First-run scripts with conditional logic

When modifying these, always test both macOS and Linux paths.

## Performance Optimizations

### Faster Chezmoi Operations
```bash
# Skip 1Password prompts when testing
chezmoi --ignore=onepassword <command>

# Parallelize package installs (when safe)
CHEZMOI_PARALLEL_PACKAGES=4 chezmoi apply

# Skip scripts during testing
chezmoi --ignore=scripts <command>
```

## Troubleshooting Crush-Specific Issues

### Template Rendering Failures
If `chezmoi execute-template` fails:
1. Check for missing variables: `chezmoi data | jq 'keys'`
2. Verify template syntax: `chezmoi execute-template --debug <file>`
3. Check 1Password connectivity if using `onepasswordRead`

### Path Migration Issues
When Homebrew path changes (e.g., `/usr/local` → `/opt/homebrew`):
1. Update `.chezmoi.yaml.tmpl` with new paths
2. Rebuild affected packages: `CHEZMOI_FORCE_REINSTALL_PACKAGES="gcc,emacs-plus" chezmoi apply`
3. Verify with `brew doctor` or `{{ .brewBin }} doctor`

### Package Installation Failures
- Check build logs in `~/.local/share/chezmoi/.work`
- Verify architecture: `chezmoi data | jq '.chezmoi.arch'`
- Check for conflicting formulas: `brew list | grep -E "(emacs|gcc)"`

## References
- **Primary Guide**: `AGENTS.md` - Universal repo fundamentals
- **User Config**: `~/GLOBAL_CRUSH.md` - Personal preferences
- **Task Index**: `CRUSH_INDEX.md` - Active task tracking
