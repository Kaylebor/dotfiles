"""Installation logic for Homebrew manager."""

from __future__ import annotations

import json
import shutil
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple, TYPE_CHECKING

import os
import subprocess

from . import log

if TYPE_CHECKING:
    from .config import PackagesHelper


class InstallService:
    """Handles package installation, environment variables, and post-install tasks."""

    MISE_TOOL_TRIGGERS: Dict[str, Set[str]] = {
        "ruby": {
            "libyaml",
            "openssl",
            "openssl@3",
            "openssl@1.1",
            "readline",
            "gmp",
            "libffi",
        }
    }

    def __init__(self, brew_bin: str, packages_file: Path) -> None:
        self.brew_bin = brew_bin
        self.packages_file = packages_file
        self._formulae_installed: List[str] = []
        self._casks_installed: List[str] = []

    # ------------------------------------------------------------------
    # Tracking helpers

    def reset_install_tracking(self) -> None:
        self._formulae_installed.clear()
        self._casks_installed.clear()

    def formulae_installed(self) -> List[str]:
        """Return list of formulae installed, preserving order."""
        seen: Set[str] = set()
        ordered: List[str] = []
        for name in self._formulae_installed:
            if name and name not in seen:
                seen.add(name)
                ordered.append(name)
        return ordered

    def casks_installed(self) -> List[str]:
        """Return list of casks installed."""
        return self._casks_installed.copy()

    def triggered_mise_tools(self) -> Dict[str, List[str]]:
        """Check which mise tools need refreshing based on installed formulae."""
        if not self._formulae_installed:
            return {}

        formulae = set(self._formulae_installed)
        triggered: Dict[str, List[str]] = {}
        for tool, watchers in self.MISE_TOOL_TRIGGERS.items():
            hits = sorted(formulae & watchers)
            if hits:
                triggered[tool] = hits
        return triggered

    # ------------------------------------------------------------------
    # Package classification

    def classify_packages(self, packages_helper: PackagesHelper) -> Tuple[List[Dict[str, Any]], List[Dict[str, Any]], List[str]]:
        """Separate packages into those needing individual install vs bundle install."""
        brews = list(packages_helper.iter_entries())
        individual: List[Dict[str, Any]] = []
        bundle: List[Dict[str, Any]] = []

        for entry in brews:
            if entry.get("type") == "string":
                bundle.append(entry)
            elif packages_helper.requires_individual(entry):
                individual.append(entry)
            else:
                bundle.append(entry)

        # Process casks with skip_if_installed support
        processed_casks: List[str] = []
        for cask in packages_helper.casks:
            if isinstance(cask, str):
                processed_casks.append(cask)
            elif isinstance(cask, dict):
                skip_paths = cask.get("skip_if_installed", [])
                paths = skip_paths if isinstance(skip_paths, list) else [skip_paths]
                if not any(Path(path).expanduser().exists() for path in paths if isinstance(path, str)):
                    name = cask.get("name")
                    if isinstance(name, str):
                        processed_casks.append(name)

        return individual, bundle, processed_casks

    # ------------------------------------------------------------------
    # Installation operations

    def install_taps(self, packages_helper: PackagesHelper) -> bool:
        """Install Homebrew taps."""
        taps = packages_helper.taps
        if not taps:
            return True

        log.step("Installing Homebrew taps...")
        for tap in taps:
            try:
                log.info(f"Tapping {tap}...")
                subprocess.run([self.brew_bin, "tap", tap], check=True)
            except subprocess.CalledProcessError as exc:
                log.error(f"Error tapping {tap}: {exc}")
                return False
        return True

    def install_individual_package(self, pkg_config: Dict[str, Any], reinstall: bool = False) -> bool:
        """Install a single package with its environment variables and args."""
        pkg_name = pkg_config.get("name") if isinstance(pkg_config, dict) else pkg_config
        if not pkg_name:
            return False

        action = "Reinstalling" if reinstall else "Installing"
        log.info(f"  {action} {pkg_name}...")

        from .config import PackagesHelper

        env = os.environ.copy()
        if isinstance(pkg_config, dict):
            base_env, _ = PackagesHelper.entry_envs(pkg_config)
            if base_env:
                env.update(base_env)
                log.info(f"    With env: {base_env}")

        cmd = [self.brew_bin, "reinstall" if reinstall else "install", pkg_name]
        if isinstance(pkg_config, dict):
            base_args = PackagesHelper.entry_args(pkg_config)
            if base_args:
                cmd.extend(f"--{arg}" for arg in base_args)

        try:
            subprocess.run(cmd, env=env, check=True)
            self._formulae_installed.append(pkg_name)
            return True
        except subprocess.CalledProcessError as exc:
            log.error(f"    Error: {exc}")
            return False

    def install_package_by_name(self, package_name: str, packages_helper: PackagesHelper) -> bool:
        """Install a package by name, looking up its config if available."""
        pkg_config = packages_helper.get_entry(package_name)

        if not pkg_config:
            log.info(f"Installing {package_name} (not in config)")
            try:
                subprocess.run([self.brew_bin, "install", package_name], check=True)
                self._formulae_installed.append(package_name)
                return True
            except subprocess.CalledProcessError as exc:
                log.error(f"Error installing {package_name}: {exc}")
                return False

        return self.install_individual_package(pkg_config, reinstall=False)

    def generate_brew_bundle(self, brews: List[Dict[str, Any]], casks: List[str]) -> str:
        """Generate Brewfile content for bundle install."""
        lines: List[str] = []
        for brew in brews:
            if brew.get("type") == "string":
                lines.append(f'brew "{brew["name"]}"')
            else:
                parts = [f'brew "{brew["name"]}"']
                base_args = brew.get("args")
                if isinstance(base_args, list):
                    args_list = ", ".join(f'"{arg}"' for arg in base_args)
                    parts.append(f"args: [{args_list}]")
                lines.append(", ".join(parts))
        for cask in casks:
            lines.append(f'cask "{cask}"')
        return "\n".join(lines)

    def install_with_bundle(
        self,
        bundle_content: str,
        brews: Iterable[Dict[str, Any]] = (),
        casks: Iterable[str] = (),
    ) -> bool:
        """Install packages using brew bundle."""
        if not bundle_content.strip():
            return True

        log.step("Installing remaining packages with brew bundle...")
        try:
            process = subprocess.Popen([self.brew_bin, "bundle", "--file=-"], stdin=subprocess.PIPE, text=True)
            process.communicate(input=bundle_content)
            if process.returncode == 0:
                log.success("Brew bundle installation completed successfully")
                for brew in brews:
                    if isinstance(brew, dict):
                        name = brew.get("name")
                    else:
                        name = brew
                    if name:
                        self._formulae_installed.append(name)
                for cask in casks:
                    if cask:
                        self._casks_installed.append(cask)
                return True
            log.error("Brew bundle installation failed")
            return False
        except Exception as exc:
            log.error(f"Error running brew bundle: {exc}")
            return False

    def install_missing_packages_only(self, packages_helper: PackagesHelper) -> bool:
        """Install only missing packages (no upgrades)."""
        log.step("Checking for missing packages...")
        try:
            result = subprocess.run([self.brew_bin, "list", "--formula"], capture_output=True, text=True, check=True)
            installed_formulae = set(result.stdout.strip().split("\n"))
            result = subprocess.run([self.brew_bin, "list", "--cask"], capture_output=True, text=True, check=True)
            installed_casks = set(result.stdout.strip().split("\n"))
        except subprocess.CalledProcessError as exc:
            log.error(f"Error getting installed packages: {exc}")
            return False

        individual, bundle, casks = self.classify_packages(packages_helper)
        missing_individual = [pkg for pkg in individual if pkg.get("name") not in installed_formulae]
        missing_bundle = [pkg for pkg in bundle if pkg.get("name") not in installed_formulae]
        missing_casks = [cask for cask in casks if cask not in installed_casks]

        for pkg in missing_individual:
            log.info(f"  Missing formula: {pkg.get('name')}")
        for cask in missing_casks:
            log.info(f"  Missing cask: {cask}")

        if missing_individual and not all(self.install_individual_package(pkg) for pkg in missing_individual):
            return False

        bundle_content = self.generate_brew_bundle(missing_bundle, missing_casks)
        if bundle_content:
            return self.install_with_bundle(bundle_content, brews=missing_bundle, casks=missing_casks)

        return True

    # ------------------------------------------------------------------
    # Post install tasks

    def setup_terminfo(self) -> bool:
        """Set up kitty TERMINFO for Emacs compatibility."""
        try:
            result = subprocess.run([self.brew_bin, "list", "--cask"], capture_output=True, text=True, check=True)
            if "kitty" not in result.stdout.splitlines():
                log.info("Skipping kitty TERMINFO setup (kitty not installed via Homebrew)")
                return True
        except subprocess.CalledProcessError:
            return True

        applications_path = "/Applications"  # Standard location
        kitty_terminfo_path = Path(applications_path) / "kitty.app" / "Contents" / "Resources" / "kitty" / "terminfo"
        if not kitty_terminfo_path.exists():
            log.warning(f"kitty terminfo directory not found: {kitty_terminfo_path}")
            return True

        log.step("Setting up kitty TERMINFO database for Emacs compatibility...")
        home_dir = Path.home()
        terminfo_dirs = [home_dir / ".terminfo" / "x", home_dir / ".terminfo" / "78"]
        for dir_path in terminfo_dirs:
            dir_path.mkdir(parents=True, exist_ok=True)

        source_file = kitty_terminfo_path / "78" / "xterm-kitty"
        if not source_file.exists():
            log.warning(f"kitty terminfo file not found at expected location: {source_file}")
            return True

        try:
            dest_file = home_dir / ".terminfo" / "x" / "xterm-kitty"
            shutil.copy2(source_file, dest_file)
            log.info("Updated kitty terminfo at ~/.terminfo/x/xterm-kitty")
            symlink_path = home_dir / ".terminfo" / "78" / "xterm-kitty"
            if symlink_path.exists() or symlink_path.is_symlink():
                symlink_path.unlink()
            symlink_path.symlink_to("../x/xterm-kitty")
            log.info("Updated symlink ~/.terminfo/78/xterm-kitty -> ~/.terminfo/x/xterm-kitty")
            log.success("Kitty TERMINFO setup complete - Emacs should work properly in terminal mode")
            return True
        except Exception as exc:
            log.error(f"Error setting up kitty TERMINFO: {exc}")
            return False

    def update_spotlight_index(self, homebrew_path: Optional[str] = None) -> bool:
        """Index ~/Applications for Spotlight (alternative Homebrew)."""
        if not homebrew_path:
            return True
        applications_dir = Path.home() / "Applications"
        if not applications_dir.exists():
            return True
        log.step("Indexing ~/Applications for Spotlight...")
        try:
            subprocess.run(["mdimport", str(applications_dir)], check=False)
        except Exception as exc:
            log.warning(f"Failed to index Applications directory: {exc}")
        return True
