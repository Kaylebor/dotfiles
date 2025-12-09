"""Rebuild logic for source-built packages with outdated dependencies."""

from __future__ import annotations

import json
import subprocess
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple

from . import log, services


def get_installed_formulae_info(brew_bin: str) -> Dict[str, Any]:
    """Get information about all installed formulae."""
    try:
        result = subprocess.run([brew_bin, "info", "--json=v2", "--installed"], capture_output=True, text=True, check=True)
        data = json.loads(result.stdout)
        # Create dictionary keyed by formula name
        formulae = {}
        for formula in data.get("formulae", []):
            name = formula.get("name")
            if name:
                formulae[name] = formula
        return formulae
    except (subprocess.CalledProcessError, json.JSONDecodeError) as e:
        log.error(f"Error fetching formulae info: {e}")
        return {}


def detect_outdated_source_builds(brew_bin: str, formulae_info: Optional[Dict[str, Any]] = None) -> List[Dict[str, Any]]:
    """Detect source-built packages with dependencies newer than themselves."""
    if formulae_info is None:
        formulae_info = get_installed_formulae_info(brew_bin)

    if not formulae_info:
        return []

    # Map formula names to install times for quick lookup
    install_times: Dict[str, Optional[str]] = {}
    for name, formula in formulae_info.items():
        installed = formula.get("installed", [])
        if installed:
            install_times[name] = installed[0].get("time")

    outdated: List[Dict[str, Any]] = []

    for name, formula in formulae_info.items():
        installed = formula.get("installed", [])
        if not installed:
            continue

        install_meta = installed[0]

        # Only consider packages built from source (not bottles)
        if install_meta.get("built_as_bottle"):
            continue

        pkg_time = install_meta.get("time")
        if not pkg_time:
            continue

        # Check if any dependencies are newer
        deps = install_meta.get("runtime_dependencies", [])
        newer_deps = []
        for dep in deps:
            dep_name = dep.get("full_name")
            dep_time = install_times.get(dep_name)
            if dep_time and dep_time > pkg_time:
                newer_deps.append(dep_name)

        if newer_deps:
            outdated.append({
                "name": name,
                "pkg_time": pkg_time,
                "newer_deps": newer_deps,
                "newer_deps_count": len(newer_deps),
                "total_deps": len(deps),
            })

    # Sort by number of outdated deps (most problematic first)
    outdated.sort(key=lambda x: x["newer_deps_count"], reverse=True)
    return outdated


def compute_rebuild_order(packages_to_rebuild: Set[str], brew_bin: str, formulae_info: Optional[Dict[str, Any]] = None) -> List[str]:
    """Compute correct rebuild order using simple topological sort.

    Instead of using NetworkX, we use brew's own dependency info
    and implement a simple topological sort algorithm.
    """
    if formulae_info is None:
        formulae_info = get_installed_formulae_info(brew_bin)

    if not formulae_info:
        log.warning("Could not fetch formulae info, using original order")
        return list(packages_to_rebuild)

    # Build dependency graph from brew info
    deps: Dict[str, Set[str]] = {}
    all_source_built: Set[str] = set()

    for name, formula in formulae_info.items():
        installed = formula.get("installed", [])
        if not installed:
            continue

        # Only consider source-built packages for dependency graph
        if installed[0].get("built_as_bottle"):
            continue

        all_source_built.add(name)
        runtime_deps = installed[0].get("runtime_dependencies", [])
        # Store only dependencies that are also source-built
        deps[name] = {d.get("full_name") for d in runtime_deps if d.get("full_name") in all_source_built}

    # Filter to only packages we want to rebuild
    packages_affected = set(packages_to_rebuild)
    for pkg in list(packages_to_rebuild):
        # Also include dependencies that need rebuilding
        if pkg in deps:
            packages_affected.update(deps[pkg])

    # Build subgraph with only affected packages
    subgraph = {pkg: deps.get(pkg, set()) & packages_affected for pkg in packages_affected}

    # Simple topological sort
    order: List[str] = []
    visited: Set[str] = set()
    temp: Set[str] = set()

    def visit(pkg: str) -> None:
        if pkg in visited:
            return
        if pkg in temp:
            log.warning(f"Circular dependency detected involving {pkg}")
            return

        temp.add(pkg)
        for dep in subgraph.get(pkg, []):
            if dep in packages_affected:
                visit(dep)
        temp.remove(pkg)
        visited.add(pkg)
        order.append(pkg)

    for pkg in packages_affected:
        if pkg not in visited:
            visit(pkg)

    # We need to rebuild dependencies first, so reverse the order
    order.reverse()
    return order


def rebuild_packages(brew_bin: str, packages: List[str], packages_helper: Any, dry_run: bool = False) -> Tuple[int, List[str]]:
    """Rebuild packages in correct dependency order.

    Returns:
        Tuple of (rebuilt_count, failed_packages)
    """
    if not packages:
        return 0, []

    if dry_run:
        log.info(f"[dry-run] Would rebuild: {', '.join(packages)}")
        return 0, []

    from .install import InstallService

    # Compute rebuild order
    order = compute_rebuild_order(set(packages), brew_bin)
    log.step(f"Rebuilding {len(order)} package(s) in dependency order...")

    # Check for running services
    running_services = services.active_services(order)
    if running_services:
        log.step(f"Will manage {len(running_services)} service(s) during rebuild")

    # Get package configs
    pkg_configs = {entry["name"]: entry for entry in packages_helper.iter_entries()}

    rebuilt = 0
    failed: List[str] = []

    for index, pkg_name in enumerate(order, start=1):
        log.step(f"\n[{index}/{len(order)}] Rebuilding {pkg_name}...")

        pkg_config = pkg_configs.get(pkg_name, {"name": pkg_name})

        # Stop service if running
        if pkg_name in running_services:
            services.stop_service(pkg_name)

        # Reinstall
        installer = InstallService(brew_bin, packages_helper.packages_file if hasattr(packages_helper, 'packages_file') else Path())
        if installer.install_individual_package(pkg_config, reinstall=True):
            rebuilt += 1
            # Restart service if it was running
            if pkg_name in running_services:
                services.start_service(pkg_name)
        else:
            failed.append(pkg_name)

    return rebuilt, failed
