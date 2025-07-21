function tv-cd-widget -d "Change directory using Television"
    set -l dir (tv dirs)
    if test -n "$dir"
        cd "$dir"
        commandline -f repaint
    end
end