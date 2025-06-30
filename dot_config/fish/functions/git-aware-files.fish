function git-aware-files
    if git rev-parse --is-inside-work-tree >/dev/null 2>&1
        # In a git repo - add colors based on git status
        begin
            # Modified/staged files with colors
            git status --porcelain | while read -l line
                set git_status (string sub -l 2 "$line")
                set file (string sub -s 4 "$line")
                
                switch "$git_status"
                    case "M "
                        set_color green; echo "$file"; set_color normal  # Staged (index modified)
                    case " M"
                        set_color yellow; echo "$file"; set_color normal  # Working tree modified
                    case "MM"
                        set_color red; echo "$file"; set_color normal     # Both staged and modified
                    case "A "
                        set_color green; echo "$file"; set_color normal   # Added/staged
                    case " A"
                        set_color cyan; echo "$file"; set_color normal    # Added in working tree
                    case "D "
                        set_color red; echo "$file"; set_color normal     # Deleted from index
                    case " D"
                        set_color magenta; echo "$file"; set_color normal # Deleted from working tree
                    case "??"
                        set_color brblack; echo "$file"; set_color normal # Untracked
                    case "*"
                        set_color blue; echo "$file"; set_color normal    # Other status
                end
            end
            
            # Clean tracked files (no color)
            git ls-files | while read -l file
                if not git status --porcelain | cut -c4- | grep -q "^$file\$"
                    echo "$file"
                end
            end
        end
    else
        # Not in git repo - use fd
        fd -u .
    end
end
