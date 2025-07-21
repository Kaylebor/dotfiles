# Company Dotfiles Repository Plan

## Executive Summary

Create a streamlined, company-focused dotfiles repository based on the proven architecture of the personal dotfiles setup, but tailored specifically for Loyal Guru's technology stack and development workflow.

## Analysis: Current Company Technology Stack

### Core Technologies (from ~/work analysis)
- **Ruby 3.2.2** - Rails API (loyal-guru-api)
- **Node.js 20** - Angular 18 app (loyal-guru-app-owner-v2)
- **Go 1.23** - Streaming services (loyal-guru-api-streaming-v2, voucher-service)
- **PostgreSQL** - Primary database
- **Redis** - Caching layer

### Development Tools in Use
- **Jest 29.7.0** - JavaScript testing
- **Firebase CLI** - Deployment and hosting
- **Prettier** - Code formatting
- **Shellcheck** - Shell script linting
- **Shfmt** - Shell script formatting
- **Docker** - Containerization
- **Google Cloud Platform** - Infrastructure

### Current Pain Points
- Developers need to manually set up development environments
- Inconsistent tool versions across team members
- ARM Mac compatibility issues (as documented in MDM_ARM_FIXES.md)
- Manual configuration of development tools
- **Alternative Homebrew path conflicts**: Personal preference (`~/.homebrew`) vs company standard (`~/homebrew`)

### Company Environment
- **Platform**: macOS exclusively (all developers use Macs)
- **Architecture**: Mix of Intel and ARM Macs
- **MDM**: Corporate device management with security restrictions
- **Security Software**: SentinelOne likely whitelists `~/homebrew` but not `~/.homebrew`

## Repository Structure Plan

### 1. Core Architecture (Keep from Personal Config)
```
company-dotfiles/
├── .chezmoi.yaml.tmpl           # Company-specific chezmoi config
├── .chezmoidata/
│   └── packages.yml             # Company-focused package list
├── run_onchange_before_brew-install-packages.bash.tmpl
├── run_onchange_98_tools_update.bash
├── dot_config/
│   ├── mise/
│   │   └── config.toml.tmpl     # Company tool versions
│   ├── fish/                    # Shell configuration
│   └── git/                     # Git configuration
├── dot_gitconfig.tmpl           # Company git settings
├── scripts/                     # Setup and maintenance scripts
└── docs/
    ├── README.md                # Company-specific setup guide
    ├── TROUBLESHOOTING.md       # ARM Mac fixes + company-specific issues
    └── DEVELOPMENT_SETUP.md     # Project-specific setup instructions
```

### 2. Tool Configuration (Based on Company Stack)

#### A. Mise Configuration (`dot_config/mise/config.toml.tmpl`)
```toml
[tools]
# Core company technologies
ruby = "3.2.2"           # Rails API
node = "20"              # Angular app
go = "1.23"              # Streaming services

# Testing and development tools
"npm:jest" = "29.7.0"
"npm:firebase-tools" = "latest"
"npm:prettier" = "latest"

# Code quality tools
"go:mvdan.cc/sh/v3/cmd/shfmt" = "latest"

# Company-specific tools
"go:github.com/loyalguru/internal-cli" = "latest"  # If exists

[env]
# Company-specific environment variables
COMPANY_NAME = "Loyal Guru"
WORKSPACE_ROOT = "{{ .chezmoi.homeDir }}/work"

# ARM Mac compatibility (from MDM_ARM_FIXES.md)
CC = "/usr/bin/clang"
CXX = "/usr/bin/clang++"
CPLUS_INCLUDE_PATH = "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"
C_INCLUDE_PATH = "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
USE_IMAGEMAGICK_6 = "1"
CFLAGS = "-Wno-error=implicit-function-declaration"

# Dynamic library paths (adapted from personal config)
{{- if eq .chezmoi.os "darwin" }}
{{- $brewPrefix := "/opt/homebrew" }}
{{- $altBrewPath := joinPath .chezmoi.homeDir .user.homebrewPath }}
{{- if stat $altBrewPath }}
{{-   $brewPrefix = $altBrewPath }}
{{- end }}
LDFLAGS = "{{ get_env "LDFLAGS" "" }} -L{{ $brewPrefix }}/opt/libpq/lib"
PKG_CONFIG_PATH = "{{ get_env "PKG_CONFIG_PATH" "" }}:{{ $brewPrefix }}/opt/libpq/lib/pkgconfig"
{{- end }}
```

#### B. Package List (`.chezmoidata/packages.yml`)
Based on the original dotfiles structure with company-specific modifications:

```yaml
packages:
  # macOS-only configuration (company uses macOS exclusively)
  darwin:
    taps:
      - "loyalguru/tap"  # If company has internal tap
    brews:
      # Core development tools
      - "git"
      - "gh"  # Note: gh has built-in jq support, no separate jq needed
      - "postgresql@17"
      - "redis"
      - "mise"
      
      # Code quality tools
      - "shellcheck"
      - "prettier"
      
      # Company-specific ARM Mac fixes (from original dotfiles)
      - name: "imagemagick@6"
        alternative_only:
          # Use original dotfiles logic for alternative Homebrew handling
          # Path is configurable: ~/homebrew (recommended) or ~/.homebrew
      - "libpq"
      - "lapack"
      - "openblas"
      
      # Essential utilities
      - "curl"
      - "yq"  # Still needed for YAML processing
      
    casks:
      - "docker"
      - "visual-studio-code"  # If company standard
      - "postman"             # For API testing
```

### 3. What to Keep from Personal Config

#### Essential Components
1. **Chezmoi templating system** - Core architecture for macOS compatibility
2. **Mise integration** - Tool version management
3. **ARM Mac compatibility fixes** - All MDM_ARM_FIXES.md solutions
4. **Fish shell configuration** - Enhanced developer experience
5. **Git configuration** - Company-specific git settings
6. **Homebrew formula patching system** - For problematic packages

#### Configuration Management
1. **macOS-specific configuration** - Intel and ARM Mac compatibility
2. **Alternative Homebrew support** - Reuse original dotfiles MDM detection logic
3. **Environment variable management** - Centralized via chezmoi templating
4. **Package installation scripts** - Adapt original `run_onchange_before_brew-install-packages.bash.tmpl`
5. **1Password integration** - Use existing detection and fallback mechanisms

### 4. What to Remove from Personal Config

#### Personal Tools
- **Emacs configuration** - Too personal, most developers use VS Code
- **Personal language tools** - Elixir, Erlang, Rust, Deno, Bun
- **Personal utilities** - bat, eza, fd, ripgrep, btop, etc.
- **Personal editor plugins** - Neovim, Helix configurations
- **Personal shell themes** - Starship, fancy prompts

#### Development Tools Not Used
- **Language runtimes** - Python, Java (unless needed for tooling)
- **Personal productivity tools** - tmux, screen configurations
- **Personal aliases and functions** - Company-specific ones only

#### Linux-Specific Configuration
- **All Linux/Arch-specific packages** - Company uses macOS exclusively
- **Linux environment detection** - Simplify to macOS-only
- **Cross-platform compatibility code** - Focus on macOS Intel/ARM differences only

### 5. Company-Specific Additions

#### A. Project Templates
```
templates/
├── rails-api/           # Rails API project setup
├── angular-app/         # Angular application setup
├── go-service/          # Go microservice setup
└── docker-compose/      # Common docker-compose configurations
```

#### B. Development Scripts
```bash
# scripts/setup-project.sh
#!/bin/bash
# Sets up a new Loyal Guru project with standard configuration

# scripts/check-environment.sh
#!/bin/bash
# Verifies all required tools are installed and properly configured

# scripts/update-tools.sh
#!/bin/bash
# Updates all development tools to company-approved versions
```

#### C. Company-Specific Documentation
- **README.md** - Company onboarding guide
- **TROUBLESHOOTING.md** - ARM Mac fixes + company-specific issues
- **DEVELOPMENT_SETUP.md** - Project-specific setup instructions
- **CODING_STANDARDS.md** - Company coding standards and tool configuration

### 6. Advanced Interactive Features

#### A. Smart 1Password Integration
Adapt the existing 1Password detection logic from the original dotfiles:

```yaml
# .chezmoi.yaml.tmpl
{{- /* Based on original dotfiles 1Password detection logic */ -}}
{{- $use1Password := false -}}
{{- if not (env "CHEZMOI_SKIP_1PASSWORD") -}}
{{-   if lookPath "op" -}}
{{-     $opTest := output "op" "account" "list" "--format=json" 2>/dev/null | fromJson -}}
{{-     if $opTest -}}
{{-       $use1Password = true -}}
{{-     end -}}
{{-   end -}}
{{- end -}}

data:
  use1Password: {{ $use1Password }}
  {{- if not $use1Password }}
  # Fallback environment variables when 1Password CLI unavailable
  gitSigningKey: {{ env "CHEZMOI_GIT_SIGNING_KEY" | default "" }}
  {{- end }}
```

#### B. Interactive User Setup
Chezmoi interactive prompts for initial configuration:

```toml
# .chezmoi.toml.tmpl
[data.user]
name = {{ promptStringOnce . "user.name" "Full name" }}
email = {{ promptStringOnce . "user.email" "Email address" }}
setupSSH = {{ promptBoolOnce . "user.setupSSH" "Set up SSH key for GitHub" }}
setupSigning = {{ promptBoolOnce . "user.setupSigning" "Use SSH key for commit signing" }}
setupGitHub = {{ promptBoolOnce . "user.setupGitHub" "Install tools based on accessible GitHub projects" }}

# Alternative Homebrew configuration
{{- $defaultBrewPath := "homebrew" }}
{{- $brewPathHelp := "Use 'homebrew' (recommended - likely whitelisted by SentinelOne) or '.homebrew' (hidden)" }}
homebrewPath = {{ promptString "Alternative Homebrew directory name" $defaultBrewPath $brewPathHelp }}

# Project selections (can be updated by rerunning chezmoi apply)
{{- if .user.setupGitHub }}
{{- $availableProjects := list -}}
{{- if lookPath "gh" -}}
{{-   /* Use gh's built-in jq support (no separate jq installation needed) */ -}}
{{-   $repoOutput := output "gh" "repo" "list" "loyalguru" "--json" "name" "--jq" ".[].name" 2>/dev/null -}}
{{-   if $repoOutput -}}
{{-     $accessibleRepos := $repoOutput | splitList "\n" | compact -}}
{{-     $knownProjects := list "loyal-guru-api" "loyal-guru-app-owner-v2" "loyal-guru-api-streaming-v2" "voucher-service" "api-docs" -}}
{{-     range $accessibleRepos -}}
{{-       if has . $knownProjects -}}
{{-         $availableProjects = append $availableProjects . -}}
{{-       end -}}
{{-     end -}}
{{-   end -}}
{{- end }}
{{- if $availableProjects }}
selectedProjects = {{ promptChoiceMultiple "Select projects to configure" $availableProjects (.user.selectedProjects | default list) }}
{{- end }}
{{- end }}
```

#### C. GitHub Integration Flow
Post-installation script sequence:

```bash
# run_after_setup-github.sh.tmpl
#!/bin/bash
set -e

echo "🔐 Setting up GitHub integration..."

# 1. GitHub CLI authentication
if ! gh auth status >/dev/null 2>&1; then
  echo "Please authenticate with GitHub:"
  gh auth login --git-protocol ssh --hostname github.com
fi

# 2. SSH key generation and setup
{{- if .user.setupSSH }}
if [ ! -f ~/.ssh/id_ed25519 ]; then
  echo "🔑 Generating SSH key..."
  ssh-keygen -t ed25519 -C "{{ .user.email }}" -f ~/.ssh/id_ed25519 -N ""
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/id_ed25519
  
  echo "📋 Adding SSH key to GitHub..."
  gh ssh-key add ~/.ssh/id_ed25519.pub --title "{{ .chezmoi.hostname }} - $(date +%Y-%m-%d)"
fi
{{- end }}

# 3. Git configuration
git config --global user.name "{{ .user.name }}"
git config --global user.email "{{ .user.email }}"

{{- if .user.setupSigning }}
git config --global user.signingkey ~/.ssh/id_ed25519.pub
git config --global commit.gpgsign true
git config --global gpg.format ssh
{{- end }}

echo "✅ GitHub integration complete!"
```

#### D. Project-Based Tool Installation
Interactive project selection with tool installation based on user choice:

```bash
# run_onchange_after_setup-projects.sh.tmpl
#!/bin/bash
set -e

{{- /* Only run if GitHub CLI is available and user opted in */ -}}
{{- if and (lookPath "gh") .user.setupGitHub -}}

echo "🔍 Checking accessible Loyal Guru projects..."

# List all repos user has access to in loyalguru org
{{- $accessibleRepos := list -}}
{{- if lookPath "gh" -}}
{{-   /* Use gh's built-in jq support (no separate jq installation needed) */ -}}
{{-   $repoOutput := output "gh" "repo" "list" "loyalguru" "--json" "name" "--jq" ".[].name" 2>/dev/null -}}
{{-   if $repoOutput -}}
{{-     $accessibleRepos = $repoOutput | splitList "\n" | compact -}}
{{-   end -}}
{{- end }}

# Known projects we can handle (only these will be offered)
{{- $knownProjects := dict
  "loyal-guru-api" (dict
    "description" "Rails API backend"
    "tools" (list "ruby" "postgresql" "redis")
  )
  "loyal-guru-app-owner-v2" (dict
    "description" "Angular frontend application"
    "tools" (list "node" "firebase-tools")
  )
  "loyal-guru-api-streaming-v2" (dict
    "description" "Go streaming API service"
    "tools" (list "go")
  )
  "voucher-service" (dict
    "description" "Go voucher microservice"
    "tools" (list "go")
  )
  "api-docs" (dict
    "description" "API documentation site"
    "tools" (list "node")
  )
-}}

# Filter to only known projects user has access to
{{- $availableProjects := list -}}
{{- range $accessibleRepos }}
{{-   if hasKey $knownProjects . }}
{{-     $availableProjects = append $availableProjects . -}}
{{-   end -}}
{{- end }}

{{- if $availableProjects }}
echo "📋 Available Loyal Guru projects you can work on:"
{{- range $availableProjects }}
{{-   $project := index $knownProjects . }}
echo "  - {{ . }}: {{ $project.description }} (tools: {{ $project.tools | join ", " }})"
{{- end }}
echo ""

# Use projects selected during chezmoi configuration
{{- $selectedProjects := .user.selectedProjects | default list }}
{{- if $selectedProjects }}
echo "🎯 Installing tools for selected projects: {{ $selectedProjects | join ", " }}"
echo ""

# Convert to bash array
SELECTED_PROJECTS=({{ range $selectedProjects }}"{{ . }}" {{ end }})
REQUIRED_TOOLS=()

echo ""
echo "📦 Selected projects and their tools:"
for project in "${SELECTED_PROJECTS[@]}"; do
  case "$project" in
{{- range $availableProjects }}
{{-   $project := index $knownProjects . }}
    "{{ . }}")
      echo "  - {{ . }}: {{ $project.tools | join " " }}"
      REQUIRED_TOOLS+=({{ $project.tools | join " " }})
      ;;
{{- end }}
    *)
      echo "  ⚠️  Unknown project: $project (skipping)"
      ;;
  esac
done

# Remove duplicates and install
if [ ${#REQUIRED_TOOLS[@]} -gt 0 ]; then
  UNIQUE_TOOLS=($(printf '%s\n' "${REQUIRED_TOOLS[@]}" | sort -u))
  echo ""
  echo "🛠️  Installing required tools: ${UNIQUE_TOOLS[*]}"
  mise install "${UNIQUE_TOOLS[@]}"
  
  echo ""
  echo "💡 To reconfigure projects, run: chezmoi apply"
else
  echo "ℹ️  No projects selected or no tools to install"
  echo "💡 To select projects, run: chezmoi apply"
fi

echo "✅ Project-based tool installation complete!"

{{- else }}
echo "ℹ️  No known Loyal Guru projects found that you have access to"
echo "Available projects: {{ keys $knownProjects | join ", " }}"
{{- end }}

{{- else }}
echo "ℹ️  Skipping project-based tool installation (GitHub CLI not available or not configured)"
{{- end }}
```

#### E. Complexity Considerations

**Benefits:**
- Seamless onboarding experience
- Automatic tool installation based on actual project access
- Smart fallbacks for corporate restrictions
- Personalized setup

**Challenges:**
- **GitHub API rate limits** - Need to handle authentication and quotas
- **Tool version conflicts** - Different projects might need different versions
- **Maintenance overhead** - Project -> tool mappings need updates
- **User choice vs automation** - Balance between convenience and control
- **Network dependencies** - Setup depends on GitHub API availability
- **Security considerations** - SSH key generation and management

**Suggested Approach:**
1. **Phase 1**: Basic interactive setup (name/email, SSH key)
2. **Phase 2**: Smart 1Password detection
3. **Phase 3**: Project-based tool installation (if proven valuable)

### 7. Implementation Strategy

#### Phase 1: Core Setup (Week 1-2)
1. **Fork personal dotfiles repository**
2. **Remove personal-specific configurations**
3. **Remove all Linux/Arch-specific code** - macOS-only simplification
4. **Keep and adapt original detection logic**:
   - MDM detection for alternative Homebrew installation
   - 1Password CLI detection and fallback mechanisms
   - ARM Mac compatibility environment variables
   - **Make Homebrew path configurable**: Default to `~/homebrew` (company standard, likely SentinelOne whitelisted) with option for `~/.homebrew` (personal preference)
5. **Update package lists** to company tools only (remove jq, gh has built-in support)
6. **Create company-specific mise configuration**
7. **Test on ARM Mac with MDM restrictions**

#### Phase 2: Company Integration (Week 3-4)
1. **Add company-specific environment variables**
2. **Create project templates** for Rails/Angular/Go
3. **Add company git configuration** (signing, hooks, etc.)
4. **Integrate with company authentication** (if applicable)
5. **Create setup scripts** for new developer onboarding

#### Phase 3: Documentation and Rollout (Week 5-6)
1. **Write comprehensive documentation**
2. **Create onboarding video/guide**
3. **Test with volunteer developers**
4. **Gather feedback and iterate**
5. **Company-wide rollout**

### 7. Key Features

#### For New Developers
- **One-command setup** - `chezmoi init --apply https://github.com/loyalguru/dotfiles`
- **Automatic tool installation** - All required tools for company projects
- **ARM Mac compatibility** - All MDM fixes pre-configured
- **Project-specific configuration** - Automatic setup for each project type

#### For Existing Developers
- **Non-invasive** - Preserves existing configurations where possible
- **Gradual adoption** - Can be applied incrementally
- **Backup and restore** - Safe migration from existing setups
- **Customizable** - Allows for personal preferences within company standards

### 8. Maintenance Strategy

#### Regular Updates
- **Monthly tool updates** - Keep company tools current
- **Quarterly review** - Assess new tools and remove deprecated ones
- **ARM Mac compatibility** - Monitor for new MDM-related issues
- **Documentation updates** - Keep troubleshooting guides current

#### Future Tool Considerations
- **Git hooks management** - Evaluate tools like Lefthook for consistent code quality
- **Advanced development tools** - Assess adoption of additional productivity tools
- **Team feedback** - Incorporate requests for new tools and workflows

#### Team Collaboration
- **Issue tracking** - GitHub issues for problems and feature requests
- **Pull request reviews** - Team input on configuration changes
- **Usage analytics** - Track adoption and identify pain points
- **Feedback loops** - Regular surveys of developer experience

### 9. Success Metrics

#### Developer Experience
- **Setup time** - Reduce new developer setup from days to hours
- **Tool consistency** - Eliminate version conflicts across team
- **ARM Mac adoption** - Enable smooth transition to ARM Macs
- **Troubleshooting time** - Reduce time spent on environment issues

#### Team Productivity
- **Onboarding speed** - Faster new developer productivity
- **Configuration drift** - Reduce inconsistencies across environments
- **Support tickets** - Decrease environment-related support requests
- **Tool adoption** - Increase usage of company-standard tools

### 10. Risk Mitigation

#### Technical Risks
- **Backup existing configurations** before applying
- **Gradual rollout** to identify issues early
- **Rollback procedures** for problematic changes
- **Testing on multiple environments** before company-wide deployment

#### Adoption Risks
- **Voluntary adoption** initially to build confidence
- **Clear documentation** to reduce learning curve
- **Support channels** for questions and issues
- **Feedback incorporation** to improve developer buy-in

## Conclusion

This plan creates a focused, company-specific dotfiles repository that leverages the proven architecture of the personal setup while removing unnecessary complexity. The result will be a streamlined developer experience that ensures consistency across the team while maintaining the flexibility to handle the unique challenges of Loyal Guru's technology stack and ARM Mac development environment.

The key is to start minimal and grow based on actual developer needs, rather than trying to solve every possible problem from the beginning. The foundation provided by the personal dotfiles architecture gives us a solid starting point for this evolution.