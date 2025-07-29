# 1Password Integration

## Overview

This dotfiles setup integrates with 1Password for secure credential management and SSH signing.

## Features

### SSH Key Management
- Git commit signing via 1Password SSH agent
- SSH authentication for remote servers
- Automatic key loading

### Template Data Access
Access 1Password items in chezmoi templates:
```go
{{ onepasswordRead "op://vault/item/field" }}
```

## Configuration

### Environment Variables

#### Skip 1Password Integration
To bypass 1Password entirely:
```bash
# In chezmoi.toml
[data]
skip1Password = true

# Or via environment
export CHEZMOI_SKIP_1PASSWORD=true
```

#### Fallback Environment Variables
When 1Password is skipped, these environment variables are used:

- `CHEZMOI_GIT_SIGNING_KEY` - Git commit signing key
- `CHEZMOI_GITHUB_API_KEY` - GitHub personal access token
- `CHEZMOI_OPENAI_API_KEY` - OpenAI API key
- `CHEZMOI_ANTHROPIC_API_KEY` - Anthropic API key
- Other `CHEZMOI_*_API_KEY` variables as needed

### Git Configuration
The git signing key is configured automatically:
```gitconfig
[user]
    signingkey = {{ .git.signingKey }}
[commit]
    gpgsign = true
[gpg]
    format = ssh
```

## Setup Instructions

### 1. Install 1Password CLI
```bash
brew install --cask 1password-cli
```

### 2. Sign in to 1Password
```bash
op signin
```

### 3. Configure SSH Agent
Enable the 1Password SSH agent in 1Password 8 settings:
1. Open 1Password 8
2. Settings → Developer
3. Enable "Use the SSH agent"
4. Enable "Authorize connections"

### 4. Test Integration
```bash
# Test SSH agent
ssh-add -l

# Test template data
chezmoi execute-template '{{ onepasswordRead "op://Personal/test/password" }}'
```

## Troubleshooting

### SSH Agent Not Working
1. Ensure 1Password 8 is running
2. Check SSH agent is enabled in 1Password settings
3. Restart terminal session

### Template Errors
1. Verify 1Password CLI is authenticated: `op whoami`
2. Check item exists: `op item get "item-name"`
3. Use `--verbose` flag: `chezmoi apply --verbose`