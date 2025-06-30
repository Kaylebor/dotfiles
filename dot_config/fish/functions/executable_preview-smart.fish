function preview-smart
    set target $argv[1]
    
    # Check if target exists as a file/directory
    if not test -e "$target"
        # Probably a command from history, just echo it
        echo "$target"
        return 0
    end
    
    # Check if it's a directory
    if test -d "$target"
        tree -C "$target"
        return 0
    end
    
    # Check if it's a regular file
    if test -f "$target"
        # Calculate line limit
        set line_limit (math $LINES - 5)
        
        # Check if file is modified by git and use difftastic
        if git status --porcelain "$target" 2>/dev/null | grep -q "^.M\|^M"
            # Create temp file with HEAD version for comparison
            set temp_file (mktemp --suffix="-$(basename "$target")")
            if git show HEAD:"$target" > "$temp_file" 2>/dev/null
                if difft --color=always --display=inline "$temp_file" "$target" 2>/dev/null
                    rm -f "$temp_file"
                    return 0
                end
                rm -f "$temp_file"
            end
        end
        
        # Get file extension (lowercase)
        set ext (string lower (path extension "$target"))
        
        # Handle JSON files with jq + bat
        if test "$ext" = ".json"
            if jq . "$target" 2>/dev/null | bat --color=always --language=json --line-range=:$line_limit --file-name="$target"
                return 0
            end
        end
        
        # Try bat for other files, limit lines
        if bat --color=always --line-range=:$line_limit "$target" 2>/dev/null
            return 0
        end
    end
    
    # Default fallback - just echo the input
    echo "$target"
end
