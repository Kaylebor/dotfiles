# GLOBAL RULES

## Tool Use

### OpenCode Tools vs Bash Commands
These are specialized tools provided by the OpenCode environment. They do NOT exist as binaries in the filesystem and CANNOT be discovered via Bash:

- **Read** - File reading tool (NOT `cat`, `head`, `tail`, or `read`)
- **Edit** - File modification tool (NOT `sed`, `awk`, or `echo >>`)
- **Write** - File creation tool (NOT `echo`, `printf`, or `cat >`)
- **Grep** - Content search tool (NOT `grep` or `rg`)
- **Glob** - File pattern matching (NOT `find` or `ls`)
- All other capitalized tools in your available toolkit

**NEVER attempt to verify these tools via Bash** - they won't appear in `ls`, `which`, `command -v`, `type`, or any other shell command.

### Tool Replacement Guide

When you need to do these tasks, use the specified tool instead of Bash:

| Task | Wrong (Bash) | Right (Tool) | Why |
|------|-------------|-------------|-----|
| Read file | `cat file`, `sed -n '10,20p'` | `Read` | Safe, formatted output, LSP integration |
| Edit file | `sed -i`, `awk` | `Edit` | Failsafes, syntax validation, atomic operations |
| Create file | `echo "content" > file` | `Write` | Proper encoding, validation hooks |
| Search content | `grep`, `rg` | `Grep` | Structured results, respects patterns |
| Find files | `find`, `ls` | `Glob` | Efficient pattern matching |

### Destructive Bash Anti-Patterns

These patterns are **explicitly forbidden** - they bypass all safety mechanisms and will break the codebase:

```bash
# ❌ NEVER USE - Destructive file operations
sed -i 's/foo/bar/' file.txt              # Use Edit instead
cat <<EOF > file.txt                      # Use Write instead  
cat <<EOF >> file.txt                     # Use Edit instead
echo "content" > file                     # Use Write instead
echo "content" >> file                    # Use Edit instead
cat file | sed ... > tmp && mv tmp file   # Use Edit instead

# ❌ NEVER USE - Temporary file workarounds
cat > /tmp/tempfile <<'EOF'
content
EOF
process /tmp/tempfile
rm /tmp/tempfile

# ❌ NEVER USE - Pipeline destruction
grep pattern file | sed 's/foo/bar/' > file.tmp && mv file.tmp file
```

**Why these are dangerous:**
- No syntax validation before writing
- No LSP integration for error checking
- Destructive operations without atomic safeguards
- Create temporary files that may not be cleaned up
- No Read-before-Write protection

### Edit/Write Constraints Are Safety Features

These "cumbersome" requirements exist **by design** - they're guardrails, not obstacles:

**1. Must Read Before Edit**
```
Error: "You must Read the file before editing"
```
**Why**: Ensures you have current file state. Prevents clobbering changes made outside OpenCode. **Design intent**: Forces proper workflow. Refresh your understanding before modifying.

**2. oldString Not Found**
```
Error: "oldString not found in file"
```
**Why**: Prevents silent failures. Text doesn't match exactly - content changed, pattern is wrong, or never existed. **Design intent**: Validation checkpoint. This error is **good** - it caught a mismatch before data loss.

**The Sed Temptation**

When agents hit these constraints, they often think: "This is annoying, I'll just use `sed -i` instead"

**This is the trap.** These constraints are protecting you. Bypassing them with `sed` is like removing guardrails because they slow you down - you'll go faster right until you careen off a cliff.

**Correct response to these errors:**
1. Read the file again (refresh your mental model)
2. Examine the actual content (find the real pattern)
3. Adjust oldString to match reality (fix your understanding)
4. Proceed with Edit (safe operation)

**When there's a built-in tool that fits what a Bash command would do, do NOT use Bash.**

## When You're Stuck

If you encounter persistent errors with Edit/Write or cannot find a pattern, **ask the User for help**. 

The User is actively monitoring your work and has extensive experience with this codebase. They can:
- Provide the correct pattern or approach
- Help debug specific template issues  
- Offer guidance on complex modifications
- Use more flexible tools when appropriate

**Do not escalate to destructive Bash workarounds** (`sed`, heredocs, temp files) when frustrated. These have repeatedly broken this codebase. Ask for help instead.
