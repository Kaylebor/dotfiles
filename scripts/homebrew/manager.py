#!/usr/bin/env -S uv run --script
# /// script
# dependencies = [
#   "pyyaml",
# ]
# ///
"""
Simplified Homebrew manager for Chezmoi.

Features:
- Parse packages.yml and install missing packages
- Detect and rebuild source-built packages with outdated dependencies
- Handle environment variables for specific packages
- Manage Homebrew services during rebuilds
- Trigger mise tool refreshes when needed
"""

import argparse
import json
import os
import subprocess
import sys
from dataclasses import dataclass
from math import inf
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple

import yaml


# =============================================================================
# Logging
# =============================================================================

def _info(msg: str, prefix: str = "") -> None:
    print(f"{prefix}{msg}")


def info(msg: str) -> None:
    _info(msg)


def success(msg: str) -> None:
    _info(msg, "✅ ")


def warning(msg: str) -> None:
    _info(msg, "⚠️  ")


def error(msg: str) -> None:
    _info(msg, "❌ ")


# =============================================================================
# Configuration
# =============================================================================

@dataclass
class PackagesConfig:
    """Parsed packages.yml content."""

    data: Dict[str, Any]

    @classmethod
    def load(cls, path: Path) -> "PackagesConfig":
        if not path.exists():
            raise FileNotFoundError(f"Packages file not found: {path}")
        try:
            with path.open("r", encoding="utf-8") as f:
                return cls(yaml.safe_load(f) or {})
        except Exception as e:
            raise RuntimeError(f"Error loading {path}: {e}")

    # Package lists
    def brews(self) -> List[Any]:
        return (self.data.get("packages", {}) or {}).get("darwin", {}).get("brews", []) or []

    def casks(self) -> List[Any]:
        return (self.data.get("packages", {}) or {}).get("darwin", {}).get("casks", []) or []

    def taps(self) -> List[str]:
        return (self.data.get("packages", {}) or {}).get("darwin", {}).get("taps", []) or []

    # Entry helpers
    def get_brew_entry(self, name: str) -> Optional[Dict[str, Any]]:
        for entry in self.brews():
            normalized = self._normalize_entry(entry)
            if normalized and normalized.get("name") == name:
                return normalized
        return None

    def iter_brew_entries(self) -> Iterable[Dict[str, Any]]:
        for entry in self.brews():
            normalized = self._normalize_entry(entry)
            if normalized:
                yield normalized

    def install_order(self, name: str) -> float:
        entry = self.get_brew_entry(name)
        if not entry:
            return inf
        try:
            return float(entry.get("install_order", inf))
        except Exception:
            return inf

    def requires_individual_install(self, entry: Dict[str, Any]) -> bool:
        """Check if package needs individual install (has env vars)."""
        return bool(entry.get("env"))

    def get_env_vars(self, entry: Dict[str, Any]) -> Dict[str, str]:
        """Get environment variables for a package."""
        env = entry.get("env")
        return dict(env) if isinstance(env, dict) else {}

    def get_args(self, entry: Dict[str, Any]) -> List[str]:
        """Get install args for a package."""
        args = entry.get("args")
        return [str(arg) for arg in args] if isinstance(args, list) else []

    @staticmethod
    def _normalize_entry(entry: Any) -> Optional[Dict[str, Any]]:
        if isinstance(entry, dict):
            return entry
        if isinstance(entry, str):
            return {"name": entry, "type": "string"}
        return None


# =============================================================================
# Brew operations
# =============================================================================

def get_brew_bin() -> str:
    """Detect brew binary location."""
    try:
        result = subprocess.run(["which", "brew"], capture_output=True, text=True, check=True)
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return "brew"  # Hope it's in PATH


def run_brew_command(args: List[str], **kwargs) -> subprocess.CompletedProcess:
    """Run a brew command, raising exception on failure."""
    brew_bin = kwargs.pop("brew_bin", get_brew_bin())
    cmd = [brew_bin] + args
    return subprocess.run(cmd, **kwargs, check=True)


def get_installed_packages(brew_bin: Optional[str] = None) -> Tuple[Set[str], Set[str]]:
    """Get sets of installed formulae and casks."""
    brew = brew_bin or get_brew_bin()

    result = run_brew_command(["list", "--formula"], brew_bin=brew, capture_output=True, text=True)
    formulae = set(result.stdout.strip().splitlines())

    result = run_brew_command(["list", "--cask"], brew_bin=brew, capture_output=True, text=True)
    casks = set(result.stdout.strip().splitlines())

    return formulae, casks


def install_taps(config: PackagesConfig, brew_bin: Optional[str] = None) -> bool:
    """Install Homebrew taps."""
    taps = config.taps()
    if not taps:
        return True

    info("Installing Homebrew taps...")
    brew = brew_bin or get_brew_bin()
    for tap in taps:
        try:
            info(f"Tapping {tap}...")
            run_brew_command(["tap", tap], brew_bin=brew)
        except subprocess.CalledProcessError as e:
            error(f"Error tapping {tap}: {e}")
            return False
    return True


def install_package(name: str, config: PackagesConfig, reinstall: bool = False, brew_bin: Optional[str] = None) -> bool:
    """Install a single package with its configuration."""
    entry = config.get_brew_entry(name)
    brew = brew_bin or get_brew_bin()

    action = "Reinstalling" if reinstall else "Installing"
    info(f"  {action} {name}...")

    cmd = [brew, "reinstall" if reinstall else "install", name]

    env = os.environ.copy()
    if entry:
        # Add environment variables if specified
        pkg_env = config.get_env_vars(entry)
        if pkg_env:
            env.update(pkg_env)
            info(f"    With env: {pkg_env}")

        # Add args if specified
        args = config.get_args(entry)
        if args:
            cmd.extend(f"--{arg}" for arg in args)

    try:
        subprocess.run(cmd, env=env, check=True, capture_output=True)
        return True
    except subprocess.CalledProcessError as e:
        error(f"    Error: {e}")
        return False


def generate_bundle_content(brews: List[Dict[str, Any]], casks: List[str]) -> str:
    """Generate Brewfile content."""
    lines: List[str] = []
    for brew in brews:
        if brew.get("type") == "string":
            lines.append(f'brew "{brew["name"]}"')
        else:
            parts = [f'brew "{brew["name"]}"']
            args = brew.get("args")
            if isinstance(args, list):
                args_list = ", ".join(f'"{arg}"' for arg in args)
                parts.append(f"args: [{args_list}]")
            lines.append(", ".join(parts))
    for cask in casks:
        lines.append(f'cask "{cask}"')
    return "\n".join(lines)


def install_with_bundle(bundle_content: str, brew_bin: Optional[str] = None) -> bool:
    """Install packages using brew bundle."""
    if not bundle_content.strip():
        return True

    info("Installing remaining packages with brew bundle...")
    brew = brew_bin or get_brew_bin()
    try:
        process = subprocess.Popen([brew, "bundle", "--file=-"], stdin=subprocess.PIPE, text=True)
        process.communicate(input=bundle_content)
        return process.returncode == 0
    except Exception as e:
        error(f"Error running brew bundle: {e}")
        return False


# =============================================================================
# Outdated detection
# =============================================================================

def get_formulae_info(brew_bin: Optional[str] = None) -> Dict[str, Any]:
    """Get detailed info about all installed formulae."""
    brew = brew_bin or get_brew_bin()
    try:
        result = run_brew_command(["info", "--json=v2", "--installed"], brew_bin=brew, capture_output=True, text=True)
        data = json.loads(result.stdout)

        # Create dictionary keyed by formula name
        formulae = {}
        for formula in data.get("formulae", []):
            name = formula.get("name")
            if name:
                formulae[name] = formula
        return formulae
    except Exception as e:
        error(f"Error fetching formulae info: {e}")
        return {}


def detect_outdated_packages(brew_bin: Optional[str] = None) -> List[Dict[str, Any]]:
    """Detect source-built packages with dependencies newer than themselves."""
    formulae = get_formulae_info(brew_bin)
    if not formulae:
        return []

    # Map names to install times for quick lookup
    install_times: Dict[str, Optional[str]] = {}
    for name, formula in formulae.items():
        installed = formula.get("installed", [])
        if installed:
            install_times[name] = installed[0].get("time")

    outdated: List[Dict[str, Any]] = []

    for name, formula in formulae.items():
        installed = formula.get("installed", [])
        if not installed:
            continue

        install_meta = installed[0]

        # Skip bottles (only rebuild source-built packages)
        if install_meta.get("built_as_bottle"):
            continue

        pkg_time = install_meta.get("time")
        if not pkg_time:
            continue

        # Check dependencies
        runtime_deps = install_meta.get("runtime_dependencies", [])
        newer_deps = []
        for dep in runtime_deps:
            dep_name = dep.get("full_name")
            dep_time = install_times.get(dep_name)
            if dep_time and dep_time > pkg_time:
                newer_deps.append(dep_name)

        if newer_deps:
            outdated.append({
                "name": name,
                "newer_deps": newer_deps,
                "newer_deps_count": len(newer_deps),
                "total_deps": len(runtime_deps),
            })

    # Sort by severity (most outdated deps first)
    outdated.sort(key=lambda x: x["newer_deps_count"], reverse=True)
    return outdated


# =============================================================================
# Topological sort (simple implementation, no NetworkX)
# =============================================================================

def topological_sort(deps: Dict[str, Set[str]], nodes: Set[str]) -> List[str]:
    """Simple topological sort using Kahn's algorithm.

    Args:
        deps: Mapping of node -> set of dependencies
        nodes: Set of nodes to sort (may be subset of deps keys)

    Returns:
        Sorted list of nodes (dependencies first)
    """
    # Build reverse dependency map
    reverse: Dict[str, Set[str]] = {node: set() for node in nodes}
    for node in nodes:
        for dep in deps.get(node, set()):
            if dep in nodes:
                reverse.setdefault(dep, set()).add(node)

    # Find nodes with no dependencies within our set
    queue: List[str] = [node for node in nodes if not (deps.get(node, set()) & nodes)]
    result: List[str] = []

    while queue:
        node = queue.pop(0)
        result.append(node)

        # Remove this node from dependencies of other nodes
        for dependent in reverse.get(node, []):
            deps[dependent].discard(node)
            if not (deps[dependent] & nodes):
                queue.append(dependent)

    # If we have cycles, nodes left in deps will have remaining dependencies
    remaining = nodes - set(result)
    if remaining:
        warning(f"Circular dependencies detected, some packages may not rebuild correctly: {', '.join(remaining)}")
        result.extend(sorted(remaining))

    return result


def compute_rebuild_order(package_names: List[str], brew_bin: Optional[str] = None) -> List[str]:
    """Compute correct order to rebuild packages based on dependencies."""
    formulae = get_formulae_info(brew_bin)
    if not formulae:
        warning("Could not fetch formulae info, using original order")
        return package_names

    # Build dependency graph from brew info
    deps: Dict[str, Set[str]] = {}
    source_built: Set[str] = set()

    for name, formula in formulae.items():
        installed = formula.get("installed", [])
        if not installed:
            continue

        # Only consider source-built packages
        if installed[0].get("built_as_bottle"):
            continue

        source_built.add(name)
        runtime_deps = installed[0].get("runtime_dependencies", [])
        deps[name] = {d.get("full_name") for d in runtime_deps if d.get("full_name") in source_built}

    # Start with packages we want to rebuild
    packages_to_rebuild = set(package_names)

    # Also include their source-built dependencies
    for pkg in list(packages_to_rebuild):
        if pkg in deps:
            packages_to_rebuild.update(deps[pkg])

    # Topological sort (dependencies first)
    order = topological_sort(deps, packages_to_rebuild)
    return order


# =============================================================================
# Services
# =============================================================================

def list_services(brew_bin: Optional[str] = None) -> Dict[str, str]:
    """Get mapping of service name -> status."""
    brew = brew_bin or get_brew_bin()
    try:
        result = run_brew_command(["services", "list"], brew_bin=brew, capture_output=True, text=True)
        services = {}
        for line in result.stdout.strip().splitlines()[1:]:  # Skip header
            parts = line.split()
            if len(parts) >= 2:
                services[parts[0]] = parts[1]
        return services
    except Exception:
        return {}


def stop_service(name: str, brew_bin: Optional[str] = None) -> bool:
    """Stop a Homebrew service."""
    info(f"  Stopping service: {name}")
    brew = brew_bin or get_brew_bin()
    try:
        run_brew_command(["services", "stop", name], brew_bin=brew, capture_output=True)
        return True
    except Exception as e:
        warning(f"  Failed to stop {name}: {e}")
        return False


def start_service(name: str, brew_bin: Optional[str] = None) -> bool:
    """Start a Homebrew service."""
    info(f"  Starting service: {name}")
    brew = brew_bin or get_brew_bin()
    try:
        run_brew_command(["services", "start", name], brew_bin=brew, capture_output=True)
        return True
    except Exception as e:
        warning(f"  Failed to start {name}: {e}")
        return False


def get_active_services(names: List[str], brew_bin: Optional[str] = None) -> Dict[str, str]:
    """Get active services from a list of service names."""
    services = list_services(brew_bin)
    return {name: status for name in names if (status := services.get(name)) in {"started", "running"}}


# =============================================================================
# Mise integration
# =============================================================================

def get_triggered_mise_tools(formulae_installed: List[str]) -> Dict[str, List[str]]:
    """Check which mise tools need refreshing based on installed formulae."""
    triggers = {
        "ruby": {"libyaml", "openssl", "openssl@3", "openssl@1.1", "readline", "gmp", "libffi"}
    }

    if not formulae_installed:
        return {}

    formulae = set(formulae_installed)
    triggered: Dict[str, List[str]] = {}
    for tool, watchers in triggers.items():
        hits = sorted(formulae & watchers)
        if hits:
            triggered[tool] = hits
    return triggered


def refresh_mise_tools(tools: List[str], *, auto_confirm: bool = False) -> bool:
    """Refresh mise-managed tools."""
    if not tools:
        return True

    # Only ruby supported for now
    tools = [t.lower() for t in tools if t.lower() == "ruby"]
    if not tools:
        info("Currently only ruby refresh is supported; skipping")
        return True

    if not shutil.which("mise"):
        warning("mise not found on PATH; skipping")
        return True

    # Get installed ruby versions
    try:
        result = subprocess.run(["mise", "list", "--json"], capture_output=True, text=True, check=True)
        data = json.loads(result.stdout or "{}")
        ruby_entries = data.get("ruby", [])
    except Exception as e:
        error(f"Failed to list mise tools: {e}")
        return False

    # Filter to global (non-project) installs
    versions = []
    for entry in ruby_entries:
        version = entry.get("version")
        contexts = entry.get("contexts", [])
        if version and not (contexts and all(ctx.get("type") == "project" for ctx in contexts)):
            versions.append(version)

    if not versions:
        info("No global ruby installations found; nothing to refresh")
        return True

    # Prompt unless auto_confirm
    if not auto_confirm:
        try:
            response = input(f"Refresh ruby versions: {', '.join(versions)}? [y/N] ").strip().lower()
            if response not in ("y", "yes"):
                info("Skipping mise refresh")
                return True
        except EOFError:
            return True

    # Refresh each version
    success_count = 0
    for version in versions:
        tool = f"ruby@{version}"
        info(f"Refreshing {tool}...")
        try:
            subprocess.run(["mise", "install", tool, "--force"], check=True, capture_output=True)
            success_count += 1
        except subprocess.CalledProcessError as e:
            error(f"Failed to refresh {tool}: {e}")

    if success_count > 0:
        info("Running mise reshim...")
        try:
            subprocess.run(["mise", "reshim"], check=True, capture_output=True)
        except subprocess.CalledProcessError as e:
            warning(f"mise reshim failed: {e}")

    success(f"Refreshed {success_count} mise tool(s)")
    return success_count > 0


# =============================================================================
# Main operations
# =============================================================================

def install_missing(config_path: Path, brew_bin: Optional[str] = None) -> bool:
    """Install only packages that are currently missing."""
    config = PackagesConfig.load(config_path)
    brew = brew_bin or get_brew_bin()

    installed_formulae, installed_casks = get_installed_packages(brew)

    # Classify packages
    individual: List[Dict[str, Any]] = []
    bundle: List[Dict[str, Any]] = []
    casks: List[str] = []

    for entry in config.iter_brew_entries():
        if config.requires_individual_install(entry):
            individual.append(entry)
        else:
            bundle.append(entry)

    for cask_entry in config.casks():
        if isinstance(cask_entry, str):
            casks.append(cask_entry)
        elif isinstance(cask_entry, dict):
            # Handle skip_if_installed
            skip_paths = cask_entry.get("skip_if_installed", [])
            paths = skip_paths if isinstance(skip_paths, list) else [skip_paths]
            if not any(Path(p).expanduser().exists() for p in paths if isinstance(p, str)):
                name = cask_entry.get("name")
                if name:
                    casks.append(name)

    # Filter to missing packages
    missing_individual = [pkg for pkg in individual if pkg.get("name") not in installed_formulae]
    missing_bundle = [pkg for pkg in bundle if pkg.get("name") not in installed_formulae]
    missing_casks = [c for c in casks if c not in installed_casks]

    # Install packages with env vars individually
    for pkg in missing_individual:
        if not install_package(pkg.get("name", ""), config, brew_bin=brew):
            return False

    # Install rest with bundle
    bundle_content = generate_bundle_content(missing_bundle, missing_casks)
    if bundle_content:
        if not install_with_bundle(bundle_content, brew):
            return False

    return True


def full_install(config_path: Path, brew_bin: Optional[str] = None) -> bool:
    """Install all packages (initial setup)."""
    config = PackagesConfig.load(config_path)
    brew = brew_bin or get_brew_bin()

    # Install taps
    if not install_taps(config, brew):
        return False

    # Classify packages
    individual: List[Dict[str, Any]] = []
    bundle: List[Dict[str, Any]] = []
    casks: List[str] = []

    for entry in config.iter_brew_entries():
        if config.requires_individual_install(entry):
            individual.append(entry)
        else:
            bundle.append(entry)

    for cask_entry in config.casks():
        if isinstance(cask_entry, str):
            casks.append(cask_entry)
        elif isinstance(cask_entry, dict) and "name" in cask_entry:
            casks.append(cask_entry["name"])

    # Sort by install_order
    individual.sort(key=lambda p: (config.install_order(p.get("name", "")), p.get("name", "")))
    bundle.sort(key=lambda p: (config.install_order(p.get("name", "")), p.get("name", "")))

    # Install packages needing individual handling first
    for pkg in individual:
        if not install_package(pkg.get("name", ""), config, brew_bin=brew):
            return False

    # Install rest with bundle
    bundle_content = generate_bundle_content(bundle, casks)
    if bundle_content:
        if not install_with_bundle(bundle_content, brew):
            return False

    return True


def check_outdated(brew_bin: Optional[str] = None) -> bool:
    """Check for source-built packages with outdated dependencies."""
    outdated = detect_outdated_packages(brew_bin)
    if not outdated:
        success("No source-built packages have outdated dependencies")
        return True

    warning(f"Found {len(outdated)} source-built packages with outdated dependencies:\n")
    for pkg in outdated:
        info(f"• {pkg['name']} ({pkg['newer_deps_count']}/{pkg['total_deps']} dependencies newer)")
        if pkg["newer_deps"]:
            info(f"  Examples: {', '.join(pkg['newer_deps'][:3])}")

    return False


def rebuild_outdated(config_path: Path, brew_bin: Optional[str] = None, *, auto_confirm: bool = False) -> bool:
    """Rebuild all outdated source-built packages."""
    config = PackagesConfig.load(config_path)
    brew = brew_bin or get_brew_bin()

    outdated = detect_outdated_packages(brew)
    if not outdated:
        success("No packages need rebuilding")
        return True

    pkg_names = [pkg["name"] for pkg in outdated]
    return rebuild_packages(config_path, pkg_names, brew, auto_confirm=auto_confirm)


def rebuild_packages(config_path: Path, package_names: List[str], brew_bin: Optional[str] = None, *, auto_confirm: bool = False) -> bool:
    """Rebuild specific packages in correct dependency order."""
    config = PackagesConfig.load(config_path)
    brew = brew_bin or get_brew_bin()

    # Compute rebuild order
    order = compute_rebuild_order(package_names, brew)
    info(f"Rebuilding {len(order)} package(s) in dependency order...")

    # Check for running services
    active = get_active_services(order, brew)
    if active:
        info(f"Will manage {len(active)} service(s) during rebuild")

    # Rebuild each package
    rebuilt = 0
    failed: List[str] = []

    for i, pkg_name in enumerate(order, 1):
        info(f"\n[{i}/{len(order)}] Rebuilding {pkg_name}...")

        # Stop service if running
        if pkg_name in active:
            stop_service(pkg_name, brew)

        # Reinstall
        if install_package(pkg_name, config, reinstall=True, brew_bin=brew):
            rebuilt += 1
            # Restart service
            if pkg_name in active:
                start_service(pkg_name, brew)
        else:
            failed.append(pkg_name)

    # Refresh mise tools if anything was rebuilt
    if rebuilt > 0:
        formulae_installed = []
        for pkg_name in order:
            if pkg_name not in failed:
                formulae_installed.append(pkg_name)
        triggered = get_triggered_mise_tools(formulae_installed)
        if triggered:
            info("\nDetected Homebrew changes that impact mise-managed tools:")
            for tool, deps in triggered.items():
                info(f"  • {tool} (due to: {', '.join(deps)})")
            refresh_mise_tools(list(triggered.keys()), auto_confirm=auto_confirm)

    if failed:
        error(f"Failed to rebuild {len(failed)} packages: {', '.join(failed)}")
        return False

    success(f"Successfully rebuilt {rebuilt} packages")
    return True


# =============================================================================
# CLI
# =============================================================================

def main() -> int:
    parser = argparse.ArgumentParser(description="Simplified Homebrew manager for Chezmoi")
    parser.add_argument("command", choices=["install", "full-install", "check-outdated", "rebuild-outdated", "rebuild"], help="Command to execute")
    parser.add_argument("--packages-file", type=Path, help="Path to packages.yml (default: <source>/.chezmoidata/packages.yml)")
    parser.add_argument("--brew-bin", help="Path to brew binary")
    parser.add_argument("--yes", action="store_true", help="Auto-confirm prompts")
    parser.add_argument("packages", nargs="*", help="Package names (for rebuild command)")

    args = parser.parse_args()

    # Default packages file location
    if args.packages_file:
        config_path = args.packages_file
    else:
        # Default: chezmoi source dir / .chezmoidata / packages.yml
        # When run via chezmoi, CHEZMOI_SOURCE_DIR is set
        source_dir = Path(os.environ.get("CHEZMOI_SOURCE_DIR", Path.cwd()))
        config_path = source_dir / ".chezmoidata" / "packages.yml"

    if not config_path.exists():
        error(f"Packages file not found: {config_path}")
        return 1

    brew = args.brew_bin or get_brew_bin()

    try:
        if args.command == "install":
            return 0 if install_missing(config_path, brew) else 1
        elif args.command == "full-install":
            return 0 if full_install(config_path, brew) else 1
        elif args.command == "check-outdated":
            return 0 if check_outdated(brew) else 1
        elif args.command == "rebuild-outdated":
            return 0 if rebuild_outdated(config_path, brew, auto_confirm=args.yes) else 1
        elif args.command == "rebuild":
            if not args.packages:
                error("rebuild command requires package names")
                return 1
            return 0 if rebuild_packages(config_path, args.packages, brew, auto_confirm=args.yes) else 1
        else:
            error(f"Unknown command: {args.command}")
            return 1
    except KeyboardInterrupt:
        error("Interrupted by user")
        return 130
    except Exception as e:
        error(f"Unexpected error: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
