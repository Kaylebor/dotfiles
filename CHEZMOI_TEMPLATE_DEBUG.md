# Chezmoi Template Emacs Support - Debug Notes

## Problem
YAML.tmpl files (and other .tmpl files) show mixed syntax highlighting:
- Template parts ({{ .var }}) appear white (no highlighting)
- YAML parts are colored
- Goal: Both parts should have proper syntax highlighting without interfering with each other

## Current Implementation
Location: `~/.config/emacs/chezmoi-templates.el`

### What We Built
1. **Multi-language support** using tree-sitter parsers:
   - `gotmpl` parser for Go template syntax
   - Embedded language parser (yaml, bash, etc.) for the base content

2. **Range separation**:
   - `chezmoi-template-find-template-ranges`: Finds all `{{...}}` and `{%...%}` regions
   - `chezmoi-template-find-non-template-ranges`: Finds everything else
   - Each parser only sees its appropriate ranges via `treesit-parser-set-included-ranges`

3. **Auto-detection**: Based on filename before `.tmpl` extension

## What We've Tried

### Attempt 1: Using treesit-range-rules
```elisp
(setq-local treesit-range-settings
            (treesit-range-rules
             :embed embed-lang
             :host 'gotmpl
             '((text) @capture)))
```
**Result**: Didn't work - the gotmpl grammar may not have "text" nodes

### Attempt 2: Manual range calculation
- Calculate template vs non-template ranges with regex
- Set included ranges on each parser separately
- Set embedded language parser as primary for font-lock
**Current Status**: This is the active implementation

## Debug Tools Available

### In chezmoi-templates.el:
- `chezmoi-template-debug-ranges`: Interactive function to inspect parser ranges
- Shows which parsers are active and their assigned ranges
- Explores gotmpl tree structure

### To test from Emacs:
1. Open a .yaml.tmpl file
2. Run `M-x chezmoi-template-debug-ranges`
3. Check *Messages* buffer for parser info

## Known Issues

1. **gotmpl grammar**: May lack font-lock rules (template parts show white)
2. **Grammar installation**: gotmpl must be installed via:
   ```elisp
   M-x treesit-install-language-grammar RET gotmpl RET
   ```

## Next Steps to Try

### From Emacs with MCP Tools:

1. **Inspect actual node types**:
   ```elisp
   (treesit-explore-mode)  ; Interactive tree explorer
   ```

2. **Check parser ranges directly**:
   ```elisp
   (dolist (parser (treesit-parser-list))
     (message "Parser %s ranges: %s" 
              (treesit-parser-language parser)
              (treesit-parser-included-ranges parser)))
   ```

3. **Test if ranges are being respected**:
   ```elisp
   ;; At a template position
   (treesit-language-at-point-function (point))  ; Should return 'gotmpl
   
   ;; At a YAML position  
   (treesit-language-at-point-function (point))  ; Should return 'yaml
   ```

4. **Add font-lock for gotmpl** (if template parts remain white):
   ```elisp
   (setq-local treesit-font-lock-settings
               (append
                (treesit-font-lock-rules
                 :language 'gotmpl
                 :feature 'delimiter
                 '((["{{" "}}" "{{-" "-}}" "{%" "%}" "{%-" "-%}"] @font-lock-preprocessor-face))
                 :feature 'variable
                 '((variable) @font-lock-variable-name-face)
                 :feature 'function
                 '((function_call) @font-lock-function-name-face))
                treesit-font-lock-settings))
   ```

## Files Modified
- `/home/kay/.local/share/chezmoi/dot_config/emacs/init.el.tmpl` - Added claude-code-ide, standardized on nerd-icons
- `/home/kay/.local/share/chezmoi/dot_config/emacs/syntax.el.tmpl` - Added gotmpl grammar source
- `/home/kay/.local/share/chezmoi/dot_config/emacs/chezmoi-templates.el` - New file for .tmpl support

## Summary for Claude Code IDE in Emacs
When you connect via Claude Code IDE, you'll have MCP tools to directly interact with Emacs. The main issue is that .tmpl files aren't properly separating template syntax from embedded language syntax. The current approach manually calculates ranges but the syntax highlighting is still mixed (template parts white, embedded parts colored).

The implementation is in `chezmoi-templates.el`. Use the debug function and treesit-explore-mode to investigate further.