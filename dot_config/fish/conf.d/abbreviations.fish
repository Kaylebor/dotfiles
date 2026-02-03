# Git abbreviations using git aliases as source of truth
# Complex logic is in ~/.config/git/config, these are just shortcuts

# Status
abbr -a -- gg 'git s'           # git s = status -sb
abbr -a -- ggg 'git status'     # full status
abbr -a -- ggp 'git status --porcelain'  # machine-readable status

# Diff
abbr -a -- gd 'git d'           # git d = diff
abbr -a -- gdd 'git dd'         # git dd = diff origin/HEAD (vs default branch)
abbr -a -- gdc 'git dc'         # git dc = diff --cached
abbr -a -- gdn 'git dn'         # git dn = diff --no-ext-diff

# Add
abbr -a -- ga 'git add'
abbr -a -- ga. 'git add .'
abbr -a -- gaa 'git aa'         # git aa = add --all

# Commit
abbr -a -- gc 'git commit'
abbr -a -- gcm 'git cm'         # git cm = commit -m
abbr -a --set-cursor='%' -- 'gcm.' 'git cm "%"'  # with cursor placement
abbr -a -- gca 'git ca'         # git ca = commit --amend
abbr -a -- gcan 'git can'       # git can = commit --amend --no-edit

# Switch branches
abbr -a -- gs 'git sw'          # git sw = switch
abbr -a -- gsw 'git sw'         # alternative: switch
abbr -a -- gsm 'git swd'        # git swd = switch to default branch (main/master)
abbr -a -- gsp 'git swp'        # git swp = switch - (previous branch)
abbr -a -- gsmain 'git switch main'
abbr -a -- gsmaster 'git switch master'

# Fetch
abbr -a -- gf 'git fetch'
abbr -a -- gfd 'git fd'         # git fd = fetch origin HEAD

# Pull
abbr -a -- gl 'git pull'
abbr -a -- gld 'git pld'        # git pld = pull from default branch

# Push
abbr -a -- gp 'git push'
abbr -a -- gpf 'git pf'         # git pf = push --force-with-lease
abbr -a -- 'gp!' 'git pf'

# Rebase
abbr -a -- gr 'git rebase'
abbr -a -- grd 'git rd'         # git rd = rebase origin/HEAD
abbr -a -- gra 'git rebase --abort'
abbr -a -- grc 'git rebase --continue'

# Merge
abbr -a -- gm 'git merge'

# Log
abbr -a -- glog 'git lg'        # git lg = log --oneline --decorate --graph
abbr -a -- gldd 'git lgd'       # git lgd = log commits not in default branch

# Show
abbr -a -- gsh 'git showd'      # git showd = show --ext-diff

# Remote
abbr -a -- grv 'git remote -v'
