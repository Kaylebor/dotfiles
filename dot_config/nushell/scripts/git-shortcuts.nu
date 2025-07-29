# Git shortcuts and functions for nushell
# Mirrors fish abbreviations as nushell aliases/functions

# Git aliases (matching fish abbreviations)
alias g = git
alias ga = git add
alias gaa = git add --all
alias gc = git commit
alias gca = git commit --amend
alias gcan = git commit --amend --no-edit
alias gcm = git commit -m
alias gd = git diff
alias gdca = git diff --cached
alias gf = git fetch
alias gg = git status
alias gl = git pull
alias glog = git log --oneline --decorate --graph
alias gm = git merge
alias gp = git push
alias "gp!" = git push --force-with-lease
alias gr = git rebase
alias gs = git show --ext-diff
alias gst = git status -sb
alias grv = git remote -v

# Interactive git checkout using tv (television)
def gco [] {
    let branch = (
        git branch -a 
        | lines 
        | where { |it| not ($it | str starts-with "*") }
        | str trim
        | str replace "remotes/origin/" ""
        | uniq
        | to text
        | tv
    )
    
    if ($branch | is-not-empty) {
        git checkout ($branch | str trim)
    }
}

# Git log with graph (extended version with optional --all flag)
def glog [--all (-a)] {
    if $all {
        git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --all
    } else {
        git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit
    }
}

# Add all and commit
def gac [message: string] {
    git add -A
    git commit -m $message
}

# Git-aware file listing (similar to fish's git-aware-files)
def gaf [] {
    let git_status = (git status --porcelain=v1 | lines)
    let files = (ls -a | where name != "." and name != "..")
    
    $files | each { |file|
        let status = ($git_status | where { |line| $line | str ends-with $file.name } | first | default "")
        let status_symbol = if ($status | str starts-with "??") {
            "?"
        } else if ($status | str starts-with " M") {
            "M"
        } else if ($status | str starts-with "A ") {
            "A"
        } else if ($status | str starts-with "D ") {
            "D"
        } else {
            ""
        }
        
        if $status_symbol != "" {
            $"($status_symbol) ($file.name)"
        } else {
            $file.name
        }
    }
}