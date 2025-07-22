# Carapace Framework Bridge Research

## Overview

This document summarizes research on the carapace framework bridges, focusing on which frameworks have the widest tool support, maintenance status, and how to identify which framework a CLI tool uses.

## Framework Popularity & Statistics

### 1. **Cobra (Go)** - Most Popular Go CLI Framework
- **GitHub Stars**: 41.2k
- **Contributors**: 315
- **Latest Release**: v1.9.1 (Feb 16, 2025)
- **Used by**: Kubernetes, Hugo, GitHub CLI, Docker, CockroachDB, Traefik
- **Key Stats**: Over 35,000 stars mentioned in multiple sources
- **Maintenance**: Very active, regular releases

### 2. **Click (Python)** - Most Popular Python CLI Framework  
- **GitHub Stars**: 16.7k
- **Contributors**: 374
- **Latest Release**: 8.2.1 (May 20, 2025)
- **Weekly Downloads**: 55,686,290
- **Projects Using**: 16,537 direct users
- **Ranking**: Key ecosystem project, top 1% of Python packages
- **Maintenance**: Healthy, maintained by Pallets organization

### 3. **Argcomplete (Python)** - Python Tab Completion
- **Weekly Downloads**: 3,886,478
- **Popularity**: More popular than 99% of all Python packages
- **Purpose**: Bash/zsh tab completion for argparse
- **Note**: Not a full CLI framework, but a completion enhancement tool

### 4. **Clap (Rust)** - Most Popular Rust CLI Framework
- **Monthly Downloads**: 21,237,301
- **Direct Usage**: 23,520 crates
- **Total Usage**: 37,882 crates
- **Ranking**: #4 in Command-line interface category
- **Maintenance**: Very active

### 5. **urfave/cli (Go)** - Alternative Go CLI Framework
- **GitHub Stars**: 23.3k
- **Contributors**: 270
- **Latest Release**: v2.27.7 (June 14)
- **Status**: Key ecosystem project
- **Versions**: v1, v2, v3 (v2/v3 actively maintained)

### 6. **Yargs (Node.js)** - Most Popular Node.js CLI Framework
- **Weekly Downloads**: 108,858,604
- **Monthly Downloads**: 271,956,643
- **NPM Ranking**: #19 overall (0.2549% of all npm downloads)
- **GitHub Stars**: 11,270
- **Projects Using**: 39,553

### 7. **Complete (Go)**
- Limited public statistics available
- Part of carapace bridge support

### 8. **Kingpin (Go)**
- Limited public statistics available
- Part of carapace bridge support

## Framework Identification Guide

### How to Identify Which Framework a CLI Tool Uses

#### 1. **Language Detection**
- **Go binaries**: Likely Cobra, urfave/cli, complete, or kingpin
- **Python scripts**: Likely Click or argcomplete
- **Node.js**: Likely Yargs
- **Rust**: Likely Clap

#### 2. **Command Structure Patterns**

**Cobra Pattern**:
```
APPNAME VERB NOUN --ADJECTIVE
APPNAME COMMAND ARG --FLAG
```
Examples: `kubectl get pods --all-namespaces`

**Click Pattern**:
- Decorator-based commands
- Often has grouped commands
- Example: `flask run --host=0.0.0.0`

**Yargs Pattern**:
- Often includes `.command()` style subcommands
- Example: `npm run build --watch`

#### 3. **Help Output Characteristics**

**Cobra**:
- Intelligent suggestions: "did you mean...?"
- Automatic help flag recognition (-h, --help)
- Structured usage/commands/flags sections

**Click**:
- Clean, minimal help output
- Options grouped by command
- Shows default values

**Yargs**:
- "Pirate-themed" messages in some cases
- Detailed command descriptions
- Shows positional arguments clearly

#### 4. **Auto-completion Behavior**

**Cobra**:
- Automatic shell completion generation
- Supports bash, zsh, fish, powershell

**Argcomplete**:
- Requires `eval "$(register-python-argcomplete PROG)"`
- Python-specific completion

**Click**:
- Completion through click-completion package
- Less automatic than Cobra

#### 5. **Source Code Indicators**

**Go imports**:
- `github.com/spf13/cobra` → Cobra
- `github.com/urfave/cli` → urfave/cli
- `github.com/alecthomas/kingpin` → Kingpin

**Python imports**:
- `import click` → Click
- `import argcomplete` → Argcomplete

**JavaScript**:
- `require('yargs')` → Yargs

## Maintenance Status

### Actively Maintained (High Activity)
1. **Cobra** - Regular releases, 315 contributors
2. **Click** - Pallets organization support, 374 contributors
3. **Clap** - Very active Rust community
4. **Yargs** - Top 20 npm package, regular updates
5. **urfave/cli** - Active development, multiple versions

### Moderate Maintenance
1. **Argcomplete** - Stable, focused purpose
2. **Complete** - Go ecosystem tool
3. **Kingpin** - Less activity than Cobra/urfave

## Recommendations for Carapace Bridges

### Priority Order (Based on Usage & Popularity)
1. **cobra** - Most Go CLIs use this
2. **click** - Dominant Python CLI framework
3. **yargs** - Dominant Node.js framework
4. **clap** - Dominant Rust framework
5. **argcomplete** - Many Python tools use argparse
6. **urfavecli** - Popular Go alternative
7. **complete**, **kingpin** - Less common but still used

### Framework Detection Strategy
1. Try framework-specific bridges first (cobra, click, etc.)
2. Fall back to shell-specific bridges (bash, zsh, fish)
3. Use language hints to narrow down framework choices
4. Check for framework-specific patterns in help output

## Carapace Configuration Best Practices

Based on the research, the optimal bridges.yaml configuration should:

1. Specify known framework bridges for popular tools
2. Use the implicit bridge fallback for unknown tools
3. Prefer framework bridges over shell bridges for better performance
4. Document which tools use which frameworks

Example configuration:
```yaml
# Framework-specific bridges (faster, more accurate)
kubectl: cobra
docker: cobra
gh: cobra
az: argcomplete
flask: click
npm: yargs

# Fallback for all other commands
_: [zsh, fish, bash]
```

## Key Insights

1. **Cobra dominates Go** - Used by most major Go CLI projects
2. **Click dominates Python** - 55M weekly downloads speaks volumes
3. **Framework bridges are superior** - Better performance and accuracy than shell bridges
4. **Active maintenance matters** - All top frameworks show healthy activity
5. **Language correlation** - Framework choice strongly correlates with programming language

This research provides a solid foundation for optimizing carapace bridge configurations and understanding the CLI framework ecosystem.