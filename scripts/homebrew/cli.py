# /// script
# dependencies = [
#   "pyyaml",
# ]
# ///
"""
Simplified Homebrew manager for Chezmoi with efficient source rebuild support.

Key improvements:
- Removed NetworkX dependency (replaced with simple topological sort)
- Removed unused features: formula patches, force_reinstall, alternative_only configs
- Single clear workflow without complex state management
- Maintains all used functionality: environment variables, mise triggers, service management
"""

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional

from .config import PackagesHelper
from .install import InstallService
from .rebuild import detect_outdated_source_builds, rebuild_packages
from .log import step, success, error, warning, info


class HomebrewManager:
    """Main orchestrator for Homebrew operations."""

    def __init__(self, packages_file: Path, *, brew_bin: Optional[str] = None, dry_run: bool = False):
        self.packages_file = packages_file
        self.dry_run = dry_run
        self.brew_bin = brew_bin or self._detect_brew_bin()
        self.packages_helper = PackagesHelper.load(packages_file)
        self.install_service = InstallService(self.brew_bin, packages_file)

    def _detect_brew_bin(self) -> str:
        """Detect brew binary location."""
        try:
            result = subprocess.run(["which", "brew"], capture_output=True, text=True, check=True)
            return result.stdout.strip()
        except subprocess.CalledProcessError:
            # Fallback to common locations
            homebrew_path = os.environ.get("HOMEBREW_PREFIX")
            if homebrew_path:
                return str(Path(homebrew_path) / "bin" / "brew")
            return "brew"  # Hope it's in PATH

    # ------------------------------------------------------------------
    # Main operations

    def install_missing(self) -> bool:
        """Install only packages that are currently missing."""
        step("Checking for missing packages...")
        return self.install_service.install_missing_packages_only(self.packages_helper)

    def full_install(self) -> bool:
        """Perform full installation of all packages."""
        step("Installing taps...")
        if not self.install_service.install_taps(self.packages_helper):
            return False

        individual, bundle, casks = self.install_service.classify_packages(self.packages_helper)

        # Sort by install_order
        def get_order(pkg):
            return (self.packages_helper.install_order(pkg.get("name")), pkg.get("name", ""))

        individual = sorted(individual, key=get_order)
        bundle = sorted(bundle, key=get_order)

        # Install packages needing individual environment handling
        if individual:
            step(f"Installing {len(individual)} package(s) with custom environment...")
            for pkg in individual:
                if not self.install_service.install_individual_package(pkg):
                    return False

        # Install remaining packages with brew bundle
        bundle_content = self.install_service.generate_brew_bundle(bundle, casks)
        if bundle_content:
            step("Installing remaining packages with brew bundle...")
            if not self.install_service.install_with_bundle(bundle_content, brews=bundle, casks=casks):
                return False

        # Post-install tasks
        if not self.install_service.setup_terminfo():
            warning("Failed to setup kitty terminfo")

        return True

    def check_outdated(self) -> bool:
        """Check for source-built packages with outdated dependencies."""
        outdated = detect_outdated_source_builds(self.brew_bin)
        if not outdated:
            success("No source-built packages have outdated dependencies")
            return True

        warning(f"Found {len(outdated)} source-built packages with outdated dependencies:\n")
        for pkg in outdated:
            info(f"• {pkg['name']} ({pkg['newer_deps_count']}/{pkg['total_deps']} deps newer)")
            if pkg['newer_deps']:
                info(f"  Newer deps: {', '.join(pkg['newer_deps'][:3])}")

        return False

    def rebuild_outdated(self) -> bool:
        """Rebuild all outdated source-built packages."""
        outdated = detect_outdated_source_builds(self.brew_bin)
        if not outdated:
            success("No packages need rebuilding")
            return True

        pkg_names = [pkg["name"] for pkg in outdated]
        rebuilt, failed = rebuild_packages(
            self.brew_bin,
            pkg_names,
            self.packages_helper,
            dry_run=self.dry_run,
        )

        if self.dry_run:
            return True

        # Handle mise triggers if anything was rebuilt
        if rebuilt > 0 and not self.dry_run:
            self._handle_mise_triggers()

        if failed:
            error(f"Failed to rebuild {len(failed)} packages: {', '.join(failed)}")
            return False

        success(f"Successfully rebuilt {rebuilt} packages")
        return True

    def rebuild_packages(self, package_names: List[str]) -> bool:
        """Rebuild specific packages."""
        rebuilt, failed = rebuild_packages(
            self.brew_bin,
            package_names,
            self.packages_helper,
            dry_run=self.dry_run,
        )

        if self.dry_run:
            return True

        if rebuilt > 0:
            self._handle_mise_triggers()

        if failed:
            error(f"Failed to rebuild {len(failed)} packages: {', '.join(failed)}")
            return False

        success(f"Successfully rebuilt {rebuilt} packages")
        return True

    def refresh_mise_tools(self, tools: List[str], *, auto_confirm: bool = False) -> bool:
        """Refresh mise-managed tools after dependency updates."""
        if not tools:
            return True

        # Only ruby is supported for now
        tools = [t.lower() for t in tools if t.lower() == "ruby"]
        if not tools:
            info("Currently only ruby refresh is supported; skipping")
            return True

        if not shutil.which("mise"):
            warning("mise not found on PATH; skipping mise tool refresh")
            return True

        # Get installed ruby versions
        try:
            result = subprocess.run(["mise", "list", "--json"], capture_output=True, text=True, check=True)
            data = json.loads(result.stdout or "{}")
            ruby_entries = data.get("ruby", [])
            if not ruby_entries:
                info("No ruby installations found in mise; nothing to refresh")
                return True
        except Exception as exc:
            error(f"Failed to get mise tool list: {exc}")
            return False

        # Filter to global installs (not project-specific)
        versions_to_refresh = []
        for entry in ruby_entries:
            version = entry.get("version")
            contexts = entry.get("contexts", [])
            # Skip project-specific installs
            if contexts and all(ctx.get("type") == "project" for ctx in contexts):
                continue
            if version:
                versions_to_refresh.append(version)

        if not versions_to_refresh:
            info("No global ruby installations found; nothing to refresh")
            return True

        # Prompt for confirmation unless auto_confirm
        if not auto_confirm:
            versions_str = ", ".join(f"ruby@{v}" for v in versions_to_refresh)
            try:
                response = input(f"Refresh mise tools: {versions_str}? [y/N] ").strip().lower()
                if response not in ("y", "yes"):
                    info("Skipping mise tool refresh")
                    return True
            except EOFError:
                info("Skipping mise tool refresh (no input)")
                return True

        # Refresh each version
        success_count = 0
        for version in versions_to_refresh:
            tool = f"ruby@{version}"
            step(f"Refreshing {tool}...")
            try:
                if not self.dry_run:
                    subprocess.run(["mise", "install", tool, "--force"], check=True)
                success_count += 1
            except subprocess.CalledProcessError as exc:
                error(f"Failed to refresh {tool}: {exc}")

        if success_count > 0 and not self.dry_run:
            step("Running mise reshim...")
            try:
                subprocess.run(["mise", "reshim"], check=True)
            except subprocess.CalledProcessError as exc:
                warning(f"mise reshim failed: {exc}")

        success(f"Refreshed {success_count} mise tool(s)")
        return success_count > 0

    def _handle_mise_triggers(self) -> bool:
        """Check and refresh mise tools if needed based on installed formulae."""
        triggered = self.install_service.triggered_mise_tools()
        if not triggered:
            return True

        info("Detected Homebrew changes that impact mise-managed tools:")
        for tool, deps in triggered.items():
            info(f"  • {tool} (due to: {', '.join(deps)})")

        return self.refresh_mise_tools(list(triggered.keys()), auto_confirm=False)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Simplified Homebrew manager for Chezmoi")
    parser.add_argument("command", choices=["install", "full-install", "check-outdated", "rebuild-outdated", "rebuild", "mise-refresh"], help="Command to execute")
    parser.add_argument("--packages-file", type=Path, help="Path to packages.yml file")
    parser.add_argument("--brew-bin", help="Path to brew binary")
    parser.add_argument("--dry-run", action="store_true", help="Show what would be done without executing")
    parser.add_argument("--auto-confirm", action="store_true", help="Auto-confirm prompts (use with caution)")
    parser.add_argument("packages", nargs="*", help="Package names for rebuild command")
    return parser


def main() -> int:
    args = build_parser().parse_args()

    # Default packages file location
    packages_file = args.packages_file or Path(__file__).parent.parent.parent / ".chezmoidata" / "packages.yml"
    if not packages_file.exists():
        error(f"Packages file not found: {packages_file}")
        return 1

    manager = HomebrewManager(packages_file, brew_bin=args.brew_bin, dry_run=args.dry_run)

    if args.dry_run:
        info(f"[DRY RUN] Executing: {args.command}")

    try:
        if args.command == "install":
            return 0 if manager.install_missing() else 1
        elif args.command == "full-install":
            return 0 if manager.full_install() else 1
        elif args.command == "check-outdated":
            return 0 if manager.check_outdated() else 1
        elif args.command == "rebuild-outdated":
            return 0 if manager.rebuild_outdated() else 1
        elif args.command == "rebuild":
            if not args.packages:
                error("rebuild command requires package names")
                return 1
            return 0 if manager.rebuild_packages(args.packages) else 1
        elif args.command == "mise-refresh":
            return 0 if manager.refresh_mise_tools([], auto_confirm=args.auto_confirm) else 1
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
