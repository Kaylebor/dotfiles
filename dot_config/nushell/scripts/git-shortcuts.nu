# Git shortcuts and functions for nushell
# Mirrors fish abbreviations as nushell aliases
# Complex logic lives in git aliases (see ~/.config/git/config), these are just shortcuts

# Core git
alias g = git

# Status
alias gg = git s          # git s = status -sb
alias ggg = git status    # full status
alias ggp = git status --porcelain  # machine-readable status

# Diff
alias gd = git d          # git d = diff
alias gdd = git dd        # git dd = diff origin/HEAD (vs default branch)
alias gdc = git dc        # git dc = diff --cached
alias gdn = git dn        # git dn = diff --no-ext-diff

# Add
alias ga = git add
alias ga. = git add .
alias gaa = git aa        # git aa = add --all

# Commit
alias gc = git commit
alias gcm = git cm        # git cm = commit -m
alias gca = git ca        # git ca = commit --amend
alias gcan = git can      # git can = commit --amend --no-edit

# Switch branches
alias gs = git sw         # git sw = switch
alias gsw = git sw        # alternative: switch
alias gsm = git swd       # git swd = switch to default branch (main/master)
alias gsp = git swp       # git swp = switch - (previous branch)
alias gsmain = git switch main
alias gsmaster = git switch master

# Fetch
alias gf = git fetch
alias gfd = git fd        # git fd = fetch origin HEAD

# Pull
alias gl = git pull
alias gld = git pld       # git pld = pull from default branch

# Push
alias gp = git push
alias gpf = git pf        # git pf = push --force-with-lease
alias gp! = git pf        # alternative: push force with lease

# Rebase
alias gr = git rebase
alias grd = git rd        # git rd = rebase origin/HEAD
alias gra = git rebase --abort
alias grc = git rebase --continue

# Merge
alias gm = git merge

# Log
alias glog = git lg       # git lg = log --oneline --decorate --graph
alias gldd = git lgd      # git lgd = log commits not in default branch

# Show
alias gsh = git showd     # git showd = show --ext-diff

# Remote
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

# Add all and commit (function can't use git aliases directly)
def gac [message: string] {
    git add -A
    git commit -m $message
}

# Switch to branch with fuzzy finder
def gsw [] {
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
        git switch ($branch | str trim)
    }
}
