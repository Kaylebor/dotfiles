function lla
    if git rev-parse --git-dir &>/dev/null
        eza --icons=auto --long --all --group --git $argv
    else
        eza --icons=auto --long --all --group $argv
    end
end