function preview-smart
    set target $argv[1]
    
    # Check if it's a directory
    if test -d "$target"
        tree -C "$target"
        return 0
    end
    
    # Check if it's a regular file
    if test -f "$target"
        # Calculate line limit
        set line_limit (math $LINES - 5)
        
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
    
    # Default fallback
    if tree -C "$target" 2>/dev/null
        return 0
    else
        echo "Cannot preview: $target"
    end
end
