function ll
    if git rev-parse --git-dir &>/dev/null
        eza --icons=auto --long --group --git $argv
    else
        eza --icons=auto --long --group $argv
    end
end