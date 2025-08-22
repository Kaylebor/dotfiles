# IDE Integration with Mise PATH Management

## Problem (macOS-specific)

On **macOS**, IDEs like Cursor don't inherit the full shell environment that includes Mise's dynamic PATH modifications. This affects:

- **AI side panels** (Cursor's AI features)
- **Language servers** (Python, Go, TypeScript, etc.)
- **Integrated terminals** (though these usually work via shell profiles)

**Note**: This is a **macOS-specific issue**. On Linux, IDEs properly inherit the shell environment, so this configuration is not needed.

## Solution: Static PATH Configuration with Chezmoi (macOS only)

This configuration uses **static PATH configuration** managed by Chezmoi to ensure all IDE processes can access Mise-managed tools on macOS.

### How It Works

1. **Static PATH**: Set `terminal.integrated.env.osx` to include Mise shims path
2. **Language Server Configuration**: Point language servers directly to Mise shims
3. **Chezmoi Management**: Use Chezmoi to manage IDE settings instead of IDE sync
4. **Platform Conditional**: Only applies on macOS (`{{- if eq .chezmoi.os "darwin" }}`)

## Setup Instructions

### Step 1: Apply Chezmoi Configuration

```bash
chezmoi apply
```

### Step 2: Restart Cursor

Restart Cursor to pick up the new settings.

## What This Provides (macOS only)

### ✅ **Terminal Access**
- Integrated terminals will have access to all Mise-managed tools
- Commands like `node`, `python`, `go`, `git` (if managed by Mise) will work

### ✅ **AI Side Panel Access**
- Cursor's AI features will be able to access Mise-managed tools
- Git commands and other tool calls from AI features will work

### ✅ **Language Server Support**
- Python language server will use Mise-managed Python
- Go language server will use Mise-managed Go
- TypeScript/Node.js tools will use Mise-managed versions

## Configuration Details

### Terminal Environment (macOS only)
```json
{
  "terminal.integrated.env.osx": {
    "PATH": "/Users/username/.local/share/mise/shims:/usr/local/bin:/usr/bin:/bin"
  }
}
```

### Language Servers (macOS only)
```json
{
  "python.defaultInterpreterPath": "/Users/username/.local/share/mise/shims/python",
  "go.goroot": "/Users/username/.local/share/mise/installs/go/latest",
  "go.gopath": "/Users/username/go"
}
```

## Platform Behavior

### macOS
- **Problem**: IDEs don't inherit full shell environment
- **Solution**: Static PATH configuration via Chezmoi
- **Result**: All IDE processes can access Mise tools

### Linux
- **Problem**: None - IDEs inherit shell environment properly
- **Solution**: No configuration needed
- **Result**: Works out of the box

## Verification

### Test Terminal Access
```bash
# In IDE terminal
which node python go git
echo $PATH | grep mise
```

### Test AI Side Panel
- In Cursor, try using AI features that call git or other tools
- Check that commands execute successfully

### Test Language Servers
- Open a Python file and verify the language server works
- Open a Go file and verify the language server works
- Check the language server output for any errors

## Maintenance

### Adding New Tools to Mise
When you add new tools to Mise, you may need to:

1. **Update language server paths** in the Chezmoi templates
2. **Apply changes**: `chezmoi apply`
3. **Restart Cursor**

### Common Tools to Configure
- **Python**: Already configured
- **Go**: Already configured  
- **Node.js**: Already configured
- **Git**: Will work if managed by Mise
- **Other tools**: Add to `terminal.integrated.env.osx` PATH if needed

## Troubleshooting

### Tools Still Not Found
1. **Check Mise shims exist**: `ls ~/.local/share/mise/shims/`
2. **Verify settings applied**: Check Cursor settings for the configured paths
3. **Restart Cursor**: Settings changes require restart
4. **Check Mise installation**: `mise ls` to see installed tools

### Language Server Issues
1. **Check tool paths**: Verify the configured paths exist
2. **Restart language servers**: Use Command Palette → "Developer: Reload Window"
3. **Check language server logs**: Look for path-related errors

## Files

- `dot_cursor/settings.json.tmpl` - Cursor settings (macOS only)
- `docs/IDE_MISE_INTEGRATION.md` - This documentation

## Benefits

- ✅ **Works for all IDE processes** (terminals, AI features, language servers)
- ✅ **Managed by Chezmoi** (version controlled, consistent across machines)
- ✅ **Simple and reliable** (no complex dynamic generation)
- ✅ **Easy to maintain** (clear configuration, easy to update)
- ✅ **No sync conflicts** (Cursor doesn't have settings sync)
- ✅ **Platform-aware** (only applies where needed)

## Limitations

- **macOS only** - Linux doesn't need this configuration
- **Manual updates required** when adding new tools to Mise
- **Static configuration** (doesn't automatically detect Mise changes)
