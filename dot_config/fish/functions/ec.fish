function ec --wraps='TERM=xterm-256color emacsclient -t' --description 'alias ec TERM=xterm-256color emacsclient -t'
    emacsclient -t $argv
end
