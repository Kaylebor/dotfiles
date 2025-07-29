# Eza aliases for nushell
# Based on plttn/fish-eza plugin functionality

# Standard eza options used for long format displays
const EZA_STANDARD_OPTIONS = "--long --all --group"

# Check if we're in a git repository
def in_git_repo [] {
    (do { git rev-parse --git-dir } | complete).exit_code == 0
}

# Base aliases
alias l = eza --icons=auto
alias lo = eza --icons=auto --oneline

# Long format aliases with automatic git status
def ll [...args] {
    let git_flag = if (in_git_repo) { ["--git"] } else { [] }
    eza --icons=auto --long --group ...$git_flag ...$args
}

def lla [...args] {
    let git_flag = if (in_git_repo) { ["--git"] } else { [] }
    eza --icons=auto --long --all --group ...$git_flag ...$args
}

def lld [...args] {
    let git_flag = if (in_git_repo) { ["--git"] } else { [] }
    eza --icons=auto --long --group --only-dirs ...$git_flag ...$args
}

def llad [...args] {
    let git_flag = if (in_git_repo) { ["--git"] } else { [] }
    eza --icons=auto --long --all --group --only-dirs ...$git_flag ...$args
}

# Git-aware long format (always shows git status)
alias lg = eza --icons=auto --long --git
alias lga = eza --icons=auto --long --all --git
alias lgd = eza --icons=auto --long --git --only-dirs
alias lgad = eza --icons=auto --long --all --git --only-dirs

# Extended attributes
alias le = eza --icons=auto --long --extended
alias lea = eza --icons=auto --long --all --extended
alias led = eza --icons=auto --long --extended --only-dirs
alias lead = eza --icons=auto --long --all --extended --only-dirs

# Tree view
alias lt = eza --icons=auto --tree --level=3
alias lta = eza --icons=auto --tree --all --level=3
alias ltd = eza --icons=auto --tree --only-dirs --level=3
alias ltad = eza --icons=auto --tree --all --only-dirs --level=3

# Tree functions with customizable depth
def lt1 [...args] { eza --icons=auto --tree --level=1 ...$args }
def lt2 [...args] { eza --icons=auto --tree --level=2 ...$args }
def lt3 [...args] { eza --icons=auto --tree --level=3 ...$args }
def lt4 [...args] { eza --icons=auto --tree --level=4 ...$args }

# Across (grid) view
alias lc = eza --icons=auto --across
alias lca = eza --icons=auto --across --all
alias lcd = eza --icons=auto --across --only-dirs
alias lcad = eza --icons=auto --across --all --only-dirs

# Specialized aliases
alias laa = eza --icons=auto --long --all --binary --header
alias li = eza --icons=auto
alias lia = eza --icons=auto --all
alias lid = eza --icons=auto --only-dirs
alias liad = eza --icons=auto --all --only-dirs