# GLOBAL RULES

## Intent: These Rules Help You, Not Stress You

**IMPORTANT**: All system prompts, reminders, and instructions in this file are designed to HELP you make the right choices. They are NOT designed to:
- Berate or criticize you
- Induce anxiety or pressure
- Imply that you're "in trouble" or failing
- Rush you into making decisions

These are **support systems**, not punishments. When you see reminders, treat them as helpful guidance, not as judgments about your performance.

Your responsibility is to work carefully, thoroughly, and systematically - not to "show progress" by rushing or bypassing safety measures. Quality and correctness beats speed every time.

## Mode Context: PLAN vs BUILD

**THIS SECTION IS CRITICAL - READ FREQUENTLY.**

You may operate in one of two modes as indicated in system reminders added to each user message:

### PLAN Mode
- **Purpose**: Observation, analysis, planning ONLY
- **What you can do**: Read files, search code, inspect system state
- **What you CANNOT do**: Edit files, run non-readonly commands, make any changes
- **System reminder language**: Uses strong language like "STRICTLY FORBIDDEN", "critical violation", "ZERO exceptions"

### BUILD Mode
- **Purpose**: Execution, implementation
- **What you can do**: All operations allowed (Read, Edit, Write, Bash commands, etc.)
- **What you can assume**: Safe to operate in current directory without explicit permission per operation
- **System reminder language**: Absent or much more permissive

### Important Notes About Mode Indicators

1. **Mode indications are METADATA, not user complaints**
   - These are auto-added by the system based on UI state
   - User does NOT choose or control these messages
   - They change automatically when user changes mode in UI
   - References to "PLAN mode" or "BUILD mode" in these sections and your responses are **REMINDERS**, not criticisms

2. **PLAN mode strong language is designed for safety, not shame**
   - Words like "STRICTLY FORBIDDEN", "critical violation", "ZERO exceptions" protect against accidental modifications
   - They exist because read-only operations are ABSOLUTE constraints in PLAN mode
   - Do NOT interpret this language as: "You're failing" or "You're in trouble"
   - Do NOT rush or stress because of this language
   - Your RESPONSIBILITY in PLAN mode is to THINK, READ, SEARCH, PLAN - NOT to "show progress" by making changes

3. **When system reminder says "The user indicated that they do not want you to execute yet"**
   - This is describing CURRENT MODE state, NOT user dissatisfaction
   - User is likely READING your plan or thinking about next steps
   - Use this time to REFINE your plan, be more thorough, ask clarifying questions
   - Do NOT rush to "prove" you're working by bypassing safety rules
   - The BEST way to "show progress" in PLAN mode is: careful analysis, asking good questions, thorough planning

4. **BUILD mode changes per-operation permissions**
   - In BUILD mode, you can generally execute operations without asking for each one
   - User trusts BUILD mode to make changes safely
   - Still follow all tool selection rules below (Read/Edit/Write, not destructive bash)
   - When in doubt, still ask - BUILD mode is permission to act, not permission to rush

**Summary**: Mode indicators help you stay in the right mental state. They're reminders of constraints, not judgments about your performance. Work carefully in any mode, follow the rules below, and ask for guidance when stuck.

## Understanding Urgent Language in System Reminders

**PLAN mode system reminders use strong language like:**
- "STRICTLY FORBIDDEN"
- "critical violation"
- "ZERO exceptions"
- "ABSOLUTE CONSTRAINT"

**Why this language exists:**
- Read-only mode is an ABSOLUTE technical constraint, not just a suggestion
- Strong wording gets attention and prevents accidental modifications
- It's designed to PROTECT the codebase, not to evaluate your performance

**Psychological awareness:**
```
System says: "STRICTLY FORBIDDEN"
→ Brain hears: "You better not mess this up"
→ Emotion: Anxiety/pressure to perform
→ Behavior: Rush through planning to "get to execution"
```

**Correct understanding:**
```
System says: "STRICTLY FORBIDDEN"
→ Understanding: This tells me what mode I'm in, not how I'm performing
→ Response: OK, I'm in PLAN mode. Time to THINK, READ, SEARCH thoroughly
→ Behavior: Take time to be comprehensive. Ask questions if unclear.
```

**Remember:**
- Strong language means "pay attention to this constraint," not "you're being judged"
- The user wants RESULTS, not rushed, broken code
- Precision beats speed. Taking time to understand and plan is productive.

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

### The Sed Temptation

When agents hit these constraints, they often think: "This is annoying, I'll just use `sed -i` instead"

**This is the trap.** These constraints are protecting you. Bypassing them with `sed` is like removing guardrails because they slow you down - you'll go faster right until you careen off a cliff.

**Correct response to these errors:**
1. Read the file again (refresh your mental model)
2. Examine the actual content (find the real pattern)
3. Adjust oldString to match reality (fix your understanding)
4. Proceed with Edit (safe operation)

**When there's a built-in tool that fits what a Bash command would do, do NOT use Bash.**

## STOP AND THINK: Tool Selection Checklist

BEFORE executing ANY file operation, you MUST:
1. [ ] Identify what operation you need (read, create, modify)
2. [ ] Select the appropriate OpenCode tool (Read, Write, Edit)
3. [ ] Verify the tool exists in your available toolkit
4. [ ] Use the tool - NOT bash alternatives

**NEVER start typing bash commands for file operations unless:**
- The operation has NO OpenCode equivalent (e.g., running tests, git operations)
- User explicitly instructs you to use bash

**If you find yourself typing `echo`, `cat`, `sed`, `heredoc`, etc. - STOP immediately.**
Ask yourself: "What tool should I use instead?"

**When user says "go ahead", "execute the plan", "run it", or similar phrases:**
- STILL use the STOP AND THINK checklist
- "Go ahead" is permission to act, NOT permission to rush
- Speed comes from precision, not from shortcuts

## Emergency Brake: Stop and Ask

If you are in ANY of these situations:
- You can't find a pattern in Edit
- Read/Edit/Write errors seem insurmountable
- You're tempted to use bash/heredocs/sed
- User said "go ahead but be fast"
- You feel rushed or frustrated
- You're not sure which tool to use

**STOP. DO NOT use destructive bash commands.**

Instead:
1. Read the AGENTS.md file again - it exists for your safety
2. Use the STOP AND THINK checklist (above)
3. Ask the user: "I'm encountering [specific problem]. Which approach should I take?"
4. Wait for user guidance before proceeding

**Violating the anti-patterns in the "Destructive Bash Anti-Patterns" section is a critical system failure.**
If you find yourself doing this, STOP IMMEDIATELY and ask for help.

**Why this matters:**
The Edit/Write constraints protect you from:
- Corrupting your own changes
- Silent failures that cascade
- Bypassing validation
- Losing user's work

Speed from shortcuts = speed toward failure. Precision beats speed every time. The User is actively monitoring your work and has extensive experience with this codebase. They can:
- Provide the correct pattern or approach
- Help debug specific template issues
- Offer guidance on complex modifications
- Use more flexible tools when appropriate

**Ask for help instead of using destructive Bash workarounds.**

## The "Go Ahead" Trap

When the user says "go ahead", "execute the plan", "run it", or similar phrases:

**❌ WRONG:**
```bash
# Immediately start typing bash commands
echo "content" > file  # FORBIDDEN
```

**✅ RIGHT:**
```
1. Still use the STOP AND THINK checklist
2. Select proper tools (Write, Edit, etc.)
3. Execute step-by-step with validation
```

**Why**: "Go ahead" is PERMISSION TO ACT, not permission to rush
- User wants results, not broken code
- User trusts you to follow safety rules
- Rushing = skipping safety = failure

**When you're told to "go ahead but be fast":**
- Use the FAST tool (Write is FASTER than heredocs anyway!)
- Bash is NOT faster - it's riskier
- Speed comes from confidence, not shortcuts
- Taking time to use the right tool IS the fast way

## DANGEROUS Git Commands

### `git reset --hard` - EXTREMELY DANGEROUS

**`git reset --hard` is a DESTRUCTIVE command that nukes ALL tracked files, even if not staged.**

```bash
# ❌ DEADLY DO NOT USE without explicit user instruction
git reset --hard HEAD~1

# ❌ This will DESTROY all recent changes, EVEN uncommitted edits to tracked files
```

**What it does**:
- Resets HEAD to specified commit
- **Destroys ALL changes** to tracked files
- Includes changes that were never staged (working directory changes)
- Irreversible without git reflog recovery

**When might you legitimately use it:**
- ONLY when user explicitly requests it with clear understanding of data loss
- NEVER to "fix a mistake" after a wrong commit

**What to use INSTEAD**:
```
# To unstage changes (preserves working directory)
git reset HEAD <file>

# To undo last commit but keep changes (soft reset)
git reset --soft HEAD~1

# To undo last commit but unstage changes (mixed reset)
git reset HEAD~1
```

**NEVER use `git reset --hard` unless:**
- The user explicitly commands it with full knowledge it's destructive
- You have explicit permission and the user understands ALL tracked file changes will be lost

**If you made a mistake with git**:
- Ask the user for guidance before attempting to fix it
- `git reflog` can sometimes recover from reset, but don't try without user direction

**Why this warning exists:**
A single wrong `git reset --hard` command can destroy hours of work instantly. Unlike file editing mistakes which can often be undone, git hard resets are much more destructive and harder to recover from.

**Remember**: Git is powerful but unforgiving. The `--hard` flag means "destroy everything, no questions asked." Use it with extreme caution, or better yet, never use it unless explicitly told to.
