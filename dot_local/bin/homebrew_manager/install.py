"""Installation and patch utilities for the Homebrew manager."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Tuple, TYPE_CHECKING

import os
import subprocess

from . import log

if TYPE_CHECKING:  # pragma: no cover
    from .config import PackagesHelper
    from typing import Any

    HomebrewManager = Any


class InstallService:
    """Encapsulates install/patch helper logic used by HomebrewManager."""

    def __init__(self, manager: "HomebrewManager") -> None:
        self.manager = manager
        self._installed_any = False

    # ------------------------------------------------------------------
    # Helpers

    def _packages(self) -> "PackagesHelper":
        return self.manager._get_packages_helper()  # noqa: SLF001

    def _is_alternative(self) -> bool:
        return self.manager._is_alternative_homebrew()  # noqa: SLF001

    # ------------------------------------------------------------------
    # High-level operations

    def install_homebrew(self) -> bool:
        config = self.manager.config
        if config.get("has_brew"):
            return True

        log.step("Installing Homebrew...")

        if self._is_alternative():
            homebrew_path = config.get("homebrew_path", "homebrew")
            tarball_url = config.get("homebrew_tarball_url")
            if not tarball_url:
                log.error("Missing homebrew_tarball_url in config")
                return False

            log.step(f"Installing Homebrew to {homebrew_path} for MDM compatibility...")
            home_dir = Path.home()
            homebrew_dir = home_dir / homebrew_path

            try:
                homebrew_dir.mkdir(parents=True, exist_ok=True)
                cmd = f'curl -L {tarball_url} | tar xz --strip-components 1 -C "{homebrew_dir}"'
                subprocess.run(cmd, shell=True, check=True)

                brew_env_cmd = f"{homebrew_dir}/bin/brew shellenv"
                result = subprocess.run(
                    brew_env_cmd,
                    shell=True,
                    capture_output=True,
                    text=True,
                    check=True,
                )

                for line in result.stdout.strip().split("\n"):
                    if line.startswith("export "):
                        key_value = line[7:]
                        if "=" in key_value:
                            key, value = key_value.split("=", 1)
                            os.environ[key] = value.strip('"\'')

                subprocess.run(["brew", "update", "--force", "--quiet"], check=True)

                brew_bin = config.get("brew_bin")
                if brew_bin:
                    prefix = subprocess.run(
                        [brew_bin, "--prefix"], capture_output=True, text=True, check=True
                    ).stdout.strip()
                    zsh_dir = Path(prefix) / "share" / "zsh"
                    if zsh_dir.exists():
                        subprocess.run(["chmod", "-R", "go-w", str(zsh_dir)], check=False)
                return True
            except subprocess.CalledProcessError as exc:
                log.error(f"Error installing alternative Homebrew: {exc}")
                return False

        install_url = config.get(
            "homebrew_install_url",
            "https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh",
        )
        try:
            cmd = f'/bin/bash -c "$(curl -fsSL {install_url})"'
            subprocess.run(cmd, shell=True, check=True)
            return True
        except subprocess.CalledProcessError as exc:
            log.error(f"Error installing standard Homebrew: {exc}")
            return False

    def verify_formula_patches(self) -> bool:
        if not self._is_alternative():
            return True

        formula_patches = self._packages().formula_patches
        if not formula_patches:
            return True

        log.step("Verifying formula patches for alternative Homebrew...")
        for formula, patch_config in formula_patches.items():
            result = subprocess.run(["brew", "list", formula], capture_output=True)
            if result.returncode == 0:
                continue

            expected_hash = (patch_config or {}).get("expected_hash")
            patch_file_path = self.manager.chezmoi_source_dir / "scripts" / "patches" / f"{formula}.patch"
            if not expected_hash and not patch_file_path.exists():
                log.info(f"Skipping hash check for {formula} (no expected_hash and no patch file)")
                continue

            log.step(f"Verifying {formula} formula hash...")
            try:
                repo_result = subprocess.run(
                    ["brew", "--repository", "homebrew/core"],
                    capture_output=True,
                    text=True,
                    check=True,
                )
                repo_path = Path(repo_result.stdout.strip())
                formula_path = repo_path / "Formula" / formula[0] / f"{formula}.rb"
                if not formula_path.exists():
                    log.warning(f"Formula not found: {formula_path}")
                    continue

                result = subprocess.run(
                    ["shasum", "-a", "256", str(formula_path)],
                    capture_output=True,
                    text=True,
                    check=True,
                )
                actual_hash = result.stdout.split()[0]
                if actual_hash != expected_hash:
                    log.error(f"Hash mismatch for {formula}!")
                    log.info(f"   Expected: {expected_hash}")
                    log.info(f"   Actual:   {actual_hash}")
                    log.info(f"   Description: {patch_config.get('description', 'No description')}")
                    issue_url = patch_config.get("issue_url")
                    if issue_url:
                        log.info(f"   Related issue: {issue_url}")
                    log.warning("   The formula may have been updated upstream. Please update the patch and expected hash.")
                    return False
                else:
                    log.success(f"{formula} formula hash verified")
                    log.info(f"   Patch: {patch_config.get('description', 'No description')}")
            except subprocess.CalledProcessError as exc:
                log.error(f"Error verifying formula {formula}: {exc}")
                return False
        return True

    def install_taps(self) -> bool:
        taps = self._packages().taps
        if not taps:
            return True

        log.step("Installing Homebrew taps...")
        for tap in taps:
            try:
                log.info(f"Tapping {tap}...")
                subprocess.run(["brew", "tap", tap], check=True)
            except subprocess.CalledProcessError as exc:
                log.error(f"Error tapping {tap}: {exc}")
                return False
        return True

    # ------------------------------------------------------------------
    # Package classification / installs

    def classify_packages(self) -> Tuple[List[Dict[str, Any]], List[Dict[str, Any]], List[str]]:
        helper = self._packages()
        brews = list(helper.iter_entries())
        individual: List[Dict[str, Any]] = []
        bundle: List[Dict[str, Any]] = []

        for entry in brews:
            if entry.get("type") == "string":
                bundle.append(entry)
            elif helper.requires_individual(entry):
                individual.append(entry)
            else:
                bundle.append(entry)

        processed_casks: List[str] = []
        for cask in helper.casks:
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

    def install_individual_packages(self, packages: Iterable[Dict[str, Any]]) -> bool:
        packages = list(packages)
        if not packages:
            self._installed_any = False
            return True
        log.step("Installing packages with isolated environments...")
        self._installed_any = True
        helper = self._packages()
        for pkg in packages:
            pkg_name = pkg.get("name") if isinstance(pkg, dict) else pkg
            if not pkg_name:
                continue
            log.info(f"Installing {pkg_name} with isolated environment...")
            env = os.environ.copy()
            base_env, alt_env = helper.entry_envs(pkg)
            if base_env:
                env.update(base_env)
                log.info(f"Applied environment variables: {base_env}")
            if self._is_alternative() and alt_env:
                env.update(alt_env)
                log.info(f"Applied alternative environment variables: {alt_env}")

            cmd = ["brew", "install", pkg_name]
            base_args = helper.entry_args(pkg)
            if base_args:
                cmd.extend(f"--{arg}" for arg in base_args)
            if self._is_alternative():
                alt_args = helper.entry_args(pkg, include_alternative=True)
                # Include only alt-specific args beyond base
                alt_specific = [arg for arg in alt_args if arg not in base_args]
                cmd.extend(f"--{arg}" for arg in alt_specific)
            try:
                subprocess.run(cmd, env=env, check=True)
                log.success(f"Successfully installed {pkg_name}")
            except subprocess.CalledProcessError as exc:
                log.error(f"Error installing {pkg_name}: {exc}")
                return False
        return True

    def install_individual_package(self, pkg_config: Dict[str, Any], reinstall: bool = False) -> bool:
        pkg_name = pkg_config.get("name") if isinstance(pkg_config, dict) else pkg_config
        if not pkg_name:
            return False

        action = "Reinstalling" if reinstall else "Installing"
        log.info(f"  {action} {pkg_name}...")
        env = os.environ.copy()
        helper = self._packages()
        if isinstance(pkg_config, dict):
            base_env, alt_env = helper.entry_envs(pkg_config)
            if base_env:
                env.update(base_env)
                log.info(f"    With env: {base_env}")
            if self._is_alternative() and alt_env:
                env.update(alt_env)
                log.info(f"    With alt env: {alt_env}")
        cmd = ["brew", "reinstall" if reinstall else "install", pkg_name]
        if isinstance(pkg_config, dict):
            base_args = helper.entry_args(pkg_config)
            if base_args:
                cmd.extend(f"--{arg}" for arg in base_args)
            if self._is_alternative():
                alt_args = helper.entry_args(pkg_config, include_alternative=True)
                alt_specific = [arg for arg in alt_args if arg not in base_args]
                cmd.extend(f"--{arg}" for arg in alt_specific)
        try:
            subprocess.run(cmd, env=env, check=True)
            return True
        except subprocess.CalledProcessError as exc:
            log.error(f"    Error: {exc}")
            return False

    def install_package_by_name(self, package_name: str) -> bool:
        helper = self._packages()
        pkg_config = helper.get_entry(package_name)

        if not pkg_config:
            log.info(f"Installing {package_name} (not in config)")
            try:
                subprocess.run(["brew", "install", package_name], check=True)
                self._installed_any = True
                return True
            except subprocess.CalledProcessError as exc:
                log.error(f"Error installing {package_name}: {exc}")
                return False

        success = self.install_individual_package(pkg_config, reinstall=False)
        if success:
            self._installed_any = True
        return success

    def generate_brew_bundle(self, brews: List[Dict[str, Any]], casks: List[str]) -> str:
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
                if self._is_alternative() and isinstance(brew.get("alternative_only"), dict):
                    alt_args = brew["alternative_only"].get("args")
                    if isinstance(alt_args, list):
                        args_list = ", ".join(f'"{arg}"' for arg in alt_args)
                        parts.append(f"args: [{args_list}]")
                lines.append(", ".join(parts))
        for cask in casks:
            lines.append(f'cask "{cask}"')
        return "\n".join(lines)

    def install_with_bundle(self, bundle_content: str) -> bool:
        if not bundle_content.strip():
            return True
        log.step("Installing remaining packages with brew bundle...")
        try:
            process = subprocess.Popen(["brew", "bundle", "--file=-"], stdin=subprocess.PIPE, text=True)
            process.communicate(input=bundle_content)
            if process.returncode == 0:
                log.success("Brew bundle installation completed successfully")
                return True
            log.error("Brew bundle installation failed")
            return False
        except Exception as exc:  # pragma: no cover - defensive
            log.error(f"Error running brew bundle: {exc}")
            return False

    def install_missing_packages_only(self) -> bool:
        log.step("Checking for missing packages...")
        try:
            result = subprocess.run(["brew", "list", "--formula"], capture_output=True, text=True, check=True)
            installed_formulae = set(result.stdout.strip().split("\n"))
            result = subprocess.run(["brew", "list", "--cask"], capture_output=True, text=True, check=True)
            installed_casks = set(result.stdout.strip().split("\n"))
        except subprocess.CalledProcessError as exc:
            log.error(f"Error getting installed packages: {exc}")
            return False

        individual, bundle, casks = self.classify_packages()
        missing_individual = [pkg for pkg in individual if pkg.get("name") not in installed_formulae]
        missing_bundle = [pkg for pkg in bundle if pkg.get("name") not in installed_formulae]
        missing_casks = [cask for cask in casks if cask not in installed_casks]

        self._installed_any = bool(missing_individual or missing_bundle or missing_casks)

        for pkg in missing_individual + missing_bundle:
            log.info(f"  Missing formula: {pkg.get('name')}")
        for cask in missing_casks:
            log.info(f"  Missing cask: {cask}")

        if missing_individual and not self.install_individual_packages(missing_individual):
            return False
        bundle_content = self.generate_brew_bundle(missing_bundle, missing_casks)
        return self.install_with_bundle(bundle_content)

    def installed_any(self) -> bool:
        return self._installed_any

    # ------------------------------------------------------------------
    # Post install tasks

    def setup_terminfo(self) -> bool:
        try:
            result = subprocess.run(["brew", "list", "--cask"], capture_output=True, text=True, check=True)
            if "kitty" not in result.stdout.splitlines():
                log.info("Skipping kitty TERMINFO setup (kitty not installed via Homebrew)")
                return True
        except subprocess.CalledProcessError:
            return True

        applications_path = self.manager.config.get("applications_path", "/Applications")
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
            import shutil

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

    def update_spotlight_index(self) -> bool:
        if not self._is_alternative():
            return True
        applications_dir = Path.home() / "Applications"
        if applications_dir.exists():
            log.step("Indexing ~/Applications for Spotlight...")
            try:
                subprocess.run(["mdimport", str(applications_dir)], check=False)
            except Exception as exc:
                log.warning(f"Failed to index Applications directory: {exc}")
        return True
