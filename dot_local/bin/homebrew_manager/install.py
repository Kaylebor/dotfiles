"""Installation and patch utilities for the Homebrew manager."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Tuple, TYPE_CHECKING

import os
import subprocess

if TYPE_CHECKING:  # pragma: no cover
    from .config import PackagesHelper
    from typing import Any

    HomebrewManager = Any


class InstallService:
    """Encapsulates install/patch helper logic used by HomebrewManager."""

    def __init__(self, manager: "HomebrewManager") -> None:
        self.manager = manager

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

        print("Installing Homebrew...")

        if self._is_alternative():
            homebrew_path = config.get("homebrew_path", "homebrew")
            tarball_url = config.get("homebrew_tarball_url")
            if not tarball_url:
                print("Error: Missing homebrew_tarball_url in config")
                return False

            print(f"Installing Homebrew to {homebrew_path} for MDM compatibility...")
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
                print(f"Error installing alternative Homebrew: {exc}")
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
            print(f"Error installing standard Homebrew: {exc}")
            return False

    def verify_formula_patches(self) -> bool:
        if not self._is_alternative():
            return True

        formula_patches = self._packages().formula_patches
        if not formula_patches:
            return True

        print("Verifying formula patches for alternative Homebrew...")
        for formula, patch_config in formula_patches.items():
            result = subprocess.run(["brew", "list", formula], capture_output=True)
            if result.returncode == 0:
                continue

            expected_hash = (patch_config or {}).get("expected_hash")
            patch_file_path = self.manager.chezmoi_source_dir / "scripts" / "patches" / f"{formula}.patch"
            if not expected_hash and not patch_file_path.exists():
                print(f"Skipping hash check for {formula} (no expected_hash and no patch file)")
                continue

            print(f"Verifying {formula} formula hash...")
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
                    print(f"⚠️  Formula not found: {formula_path}")
                    continue

                result = subprocess.run(
                    ["shasum", "-a", "256", str(formula_path)],
                    capture_output=True,
                    text=True,
                    check=True,
                )
                actual_hash = result.stdout.split()[0]
                if actual_hash != expected_hash:
                    print("❌ Hash mismatch for {formula}!")
                    print(f"   Expected: {expected_hash}")
                    print(f"   Actual:   {actual_hash}")
                    print(f"   Description: {patch_config.get('description', 'No description')}")
                    issue_url = patch_config.get("issue_url")
                    if issue_url:
                        print(f"   Related issue: {issue_url}")
                    print("   The formula may have been updated upstream.")
                    print("   Please update the patch and expected hash.")
                    return False
                else:
                    print(f"✅ {formula} formula hash verified")
                    print(f"   Patch: {patch_config.get('description', 'No description')}")
            except subprocess.CalledProcessError as exc:
                print(f"Error verifying formula {formula}: {exc}")
                return False
        return True

    def install_taps(self) -> bool:
        taps = self._packages().taps
        if not taps:
            return True

        print("Installing Homebrew taps...")
        for tap in taps:
            try:
                print(f"Tapping {tap}...")
                subprocess.run(["brew", "tap", tap], check=True)
            except subprocess.CalledProcessError as exc:
                print(f"Error tapping {tap}: {exc}")
                return False
        return True

    # ------------------------------------------------------------------
    # Package classification / installs

    def classify_packages(self) -> Tuple[List[Dict[str, Any]], List[Dict[str, Any]], List[str]]:
        helper = self._packages()
        brews = helper.brews
        casks = helper.casks

        individual: List[Dict[str, Any]] = []
        bundle: List[Dict[str, Any]] = []

        for pkg in brews:
            if isinstance(pkg, str):
                bundle.append({"name": pkg, "type": "string"})
            elif isinstance(pkg, dict):
                has_env = "env" in pkg
                has_alt_env = self._is_alternative() and isinstance(pkg.get("alternative_only"), dict) and "env" in pkg["alternative_only"]
                if has_env or has_alt_env:
                    individual.append(pkg)
                else:
                    bundle.append(pkg)

        processed_casks: List[str] = []
        for cask in casks:
            if isinstance(cask, str):
                processed_casks.append(cask)
            elif isinstance(cask, dict):
                skip_paths = cask.get("skip_if_installed", [])
                should_skip = False
                paths = skip_paths if isinstance(skip_paths, list) else [skip_paths]
                for path in paths:
                    if Path(path).expanduser().exists():
                        should_skip = True
                        break
                if not should_skip and "name" in cask:
                    processed_casks.append(cask["name"])
        return individual, bundle, processed_casks

    def install_individual_packages(self, packages: Iterable[Dict[str, Any]]) -> bool:
        packages = list(packages)
        if not packages:
            return True
        print("Installing packages with isolated environments...")
        for pkg in packages:
            pkg_name = pkg.get("name")
            if not pkg_name:
                continue
            print(f"Installing {pkg_name} with isolated environment...")
            env = os.environ.copy()
            if "env" in pkg:
                env.update(pkg["env"])
                print(f"Applied environment variables: {pkg['env']}")
            if self._is_alternative() and isinstance(pkg.get("alternative_only"), dict) and "env" in pkg["alternative_only"]:
                env.update(pkg["alternative_only"]["env"])
                print(f"Applied alternative environment variables: {pkg['alternative_only']['env']}")

            cmd = ["brew", "install", pkg_name]
            if "args" in pkg:
                cmd.extend(f"--{arg}" for arg in pkg["args"])
            if self._is_alternative() and isinstance(pkg.get("alternative_only"), dict) and "args" in pkg["alternative_only"]:
                cmd.extend(f"--{arg}" for arg in pkg["alternative_only"]["args"])
            try:
                subprocess.run(cmd, env=env, check=True)
                print(f"✅ Successfully installed {pkg_name}")
            except subprocess.CalledProcessError as exc:
                print(f"❌ Error installing {pkg_name}: {exc}")
                return False
        return True

    def install_individual_package(self, pkg_config: Dict[str, Any], reinstall: bool = False) -> bool:
        pkg_name = pkg_config.get("name") if isinstance(pkg_config, dict) else pkg_config
        if not pkg_name:
            return False

        action = "Reinstalling" if reinstall else "Installing"
        print(f"  {action} {pkg_name}...")
        env = os.environ.copy()
        if isinstance(pkg_config, dict):
            base_env = pkg_config.get("env")
            if isinstance(base_env, dict):
                env.update(base_env)
                print(f"    With env: {base_env}")
            if self._is_alternative():
                alt_env = pkg_config.get("alternative_only", {}).get("env") if isinstance(pkg_config.get("alternative_only"), dict) else None
                if isinstance(alt_env, dict):
                    env.update(alt_env)
                    print(f"    With alt env: {alt_env}")
        cmd = ["brew", "reinstall" if reinstall else "install", pkg_name]
        if isinstance(pkg_config, dict):
            base_args = pkg_config.get("args")
            if isinstance(base_args, list):
                cmd.extend(f"--{arg}" for arg in base_args)
            if self._is_alternative():
                alt_args = pkg_config.get("alternative_only", {}).get("args") if isinstance(pkg_config.get("alternative_only"), dict) else None
                if isinstance(alt_args, list):
                    cmd.extend(f"--{arg}" for arg in alt_args)
        try:
            subprocess.run(cmd, env=env, check=True)
            return True
        except subprocess.CalledProcessError as exc:
            print(f"    Error: {exc}")
            return False

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
        print("Installing remaining packages with brew bundle...")
        try:
            process = subprocess.Popen(["brew", "bundle", "--file=-"], stdin=subprocess.PIPE, text=True)
            process.communicate(input=bundle_content)
            if process.returncode == 0:
                print("✅ Brew bundle installation completed successfully")
                return True
            print("❌ Brew bundle installation failed")
            return False
        except Exception as exc:  # pragma: no cover - defensive
            print(f"❌ Error running brew bundle: {exc}")
            return False

    def install_missing_packages_only(self) -> bool:
        print("Checking for missing packages...")
        try:
            result = subprocess.run(["brew", "list", "--formula"], capture_output=True, text=True, check=True)
            installed_formulae = set(result.stdout.strip().split("\n"))
            result = subprocess.run(["brew", "list", "--cask"], capture_output=True, text=True, check=True)
            installed_casks = set(result.stdout.strip().split("\n"))
        except subprocess.CalledProcessError as exc:
            print(f"Error getting installed packages: {exc}")
            return False

        individual, bundle, casks = self.classify_packages()
        missing_individual = [pkg for pkg in individual if pkg.get("name") not in installed_formulae]
        missing_bundle = [pkg for pkg in bundle if pkg.get("name") not in installed_formulae]
        missing_casks = [cask for cask in casks if cask not in installed_casks]

        for pkg in missing_individual + missing_bundle:
            print(f"  Missing formula: {pkg.get('name')}")
        for cask in missing_casks:
            print(f"  Missing cask: {cask}")

        if missing_individual and not self.install_individual_packages(missing_individual):
            return False
        bundle_content = self.generate_brew_bundle(missing_bundle, missing_casks)
        return self.install_with_bundle(bundle_content)

    # ------------------------------------------------------------------
    # Post install tasks

    def setup_terminfo(self) -> bool:
        try:
            result = subprocess.run(["brew", "list", "--cask"], capture_output=True, text=True, check=True)
            if "kitty" not in result.stdout.splitlines():
                print("Skipping kitty TERMINFO setup (kitty not installed via Homebrew)")
                return True
        except subprocess.CalledProcessError:
            return True

        applications_path = self.manager.config.get("applications_path", "/Applications")
        kitty_terminfo_path = Path(applications_path) / "kitty.app" / "Contents" / "Resources" / "kitty" / "terminfo"
        if not kitty_terminfo_path.exists():
            print(f"⚠️  Warning: kitty terminfo directory not found: {kitty_terminfo_path}")
            return True

        print("Setting up kitty TERMINFO database for Emacs compatibility...")
        home_dir = Path.home()
        terminfo_dirs = [home_dir / ".terminfo" / "x", home_dir / ".terminfo" / "78"]
        for dir_path in terminfo_dirs:
            dir_path.mkdir(parents=True, exist_ok=True)

        source_file = kitty_terminfo_path / "78" / "xterm-kitty"
        if not source_file.exists():
            print(f"⚠️  Warning: kitty terminfo file not found at expected location: {source_file}")
            return True
        try:
            import shutil

            dest_file = home_dir / ".terminfo" / "x" / "xterm-kitty"
            shutil.copy2(source_file, dest_file)
            print("Updated kitty terminfo at ~/.terminfo/x/xterm-kitty")
            symlink_path = home_dir / ".terminfo" / "78" / "xterm-kitty"
            if symlink_path.exists() or symlink_path.is_symlink():
                symlink_path.unlink()
            symlink_path.symlink_to("../x/xterm-kitty")
            print("Updated symlink ~/.terminfo/78/xterm-kitty -> ~/.terminfo/x/xterm-kitty")
            print("✅ Kitty TERMINFO setup complete - Emacs should work properly in terminal mode")
            return True
        except Exception as exc:
            print(f"❌ Error setting up kitty TERMINFO: {exc}")
            return False

    def update_spotlight_index(self) -> bool:
        if not self._is_alternative():
            return True
        applications_dir = Path.home() / "Applications"
        if applications_dir.exists():
            print("Indexing ~/Applications for Spotlight...")
            try:
                subprocess.run(["mdimport", str(applications_dir)], check=False)
            except Exception as exc:
                print(f"Warning: Failed to index Applications directory: {exc}")
        return True
