"""Rebuild logic extracted from the Homebrew manager."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, Dict, List, Optional, Sequence, Set

import networkx as nx

from . import log


@dataclass
class RebuildResult:
    rebuilt: int
    failed: Sequence[str]


class RebuildService:
    """Encapsulate dependency detection and rebuild execution."""

    def __init__(
        self,
        *,
        get_formulae_info: Callable[[bool], Dict[str, Dict]],
        packages_helper,
        stop_service: Callable[[str], bool],
        start_service: Callable[[str], bool],
        install_package: Callable[[Dict, bool], bool],
        dry_run: bool = False,
        is_alternative: Callable[[], bool],
    ) -> None:
        self._get_formulae_info = get_formulae_info
        self._packages_helper = packages_helper
        self._stop_service = stop_service
        self._start_service = start_service
        self._install_package = install_package
        self._dry_run = dry_run
        self._is_alternative = is_alternative

    # ------------------------------------------------------------------
    # Detection

    def detect_outdated(self) -> List[Dict[str, Any]]:
        formulae = self._get_formulae_info(False)
        if not formulae:
            return []

        install_times: Dict[str, Optional[str]] = {}
        for name, formula in formulae.items():
            if "installed" in formula and formula["installed"]:
                install_times[name] = formula["installed"][0].get("time")

        outdated_packages: List[Dict[str, Any]] = []
        seen: Set[str] = set()

        for formula in formulae.values():
            pkg_name = formula.get("name")
            if not pkg_name or pkg_name in seen:
                continue
            seen.add(pkg_name)

            installed = formula.get("installed") or []
            if not installed:
                continue
            install_meta = installed[0]
            if install_meta.get("built_as_bottle"):
                continue

            pkg_time = install_meta.get("time")
            if not pkg_time:
                continue

            deps = install_meta.get("runtime_dependencies", [])
            newer_deps = []
            for dep in deps:
                dep_name = dep.get("full_name")
                dep_time = install_times.get(dep_name)
                if dep_time and dep_time > pkg_time:
                    newer_deps.append(dep_name)

            if newer_deps:
                outdated_packages.append(
                    {
                        "name": pkg_name,
                        "pkg_time": pkg_time,
                        "newer_deps": newer_deps,
                        "newer_deps_count": len(newer_deps),
                        "total_deps": len(deps),
                    }
                )

        outdated_packages.sort(key=lambda x: x["newer_deps_count"], reverse=True)
        return outdated_packages

    # ------------------------------------------------------------------
    # Rebuild execution

    def rebuild(self, outdated: List[Dict[str, Any]]) -> RebuildResult:
        if self._dry_run:
            log.info("[dry-run] RebuildService would rebuild: %s" % ", ".join(pkg["name"] for pkg in outdated))
            return RebuildResult(rebuilt=0, failed=())

        packages_to_rebuild = {pkg["name"] for pkg in outdated}
        rebuild_order = self._compute_rebuild_order(packages_to_rebuild)

        running_services = self._detect_running_services(rebuild_order)
        if running_services:
            log.step(f"\nWill manage {len(running_services)} service(s) during rebuild")

        pkg_configs = {entry["name"]: entry for entry in self._packages_helper.iter_entries()}

        failed: List[str] = []
        rebuilt_count = 0

        for index, pkg_name in enumerate(rebuild_order, start=1):
            log.step(f"\n[{index}/{len(rebuild_order)}] Rebuilding {pkg_name}...")

            pkg_config = pkg_configs.get(pkg_name, {"name": pkg_name})

            if pkg_name in running_services:
                self._stop_service(pkg_name)

            force_reinstall = self._packages_helper.force_reinstall_enabled(pkg_name)
            # Packages that always force reinstall
            if pkg_name in {"emacs-plus@31", "emacs-plus@30", "emacs-plus@29"}:
                force_reinstall = True
                log.info(f"  Note: {pkg_name} always uses force reinstall")

            if force_reinstall:
                try:
                    subprocess_run(["brew", "uninstall", "--ignore-dependencies", pkg_name])
                    log.info(f"  Uninstalled {pkg_name} for clean reinstall")
                except Exception:
                    log.warning(f"  Failed to uninstall {pkg_name}, continuing anyway...")

            if self._install_package(pkg_config, reinstall=(not force_reinstall)):
                rebuilt_count += 1
                if pkg_name in running_services:
                    self._start_service(pkg_name)
            else:
                failed.append(pkg_name)

        return RebuildResult(rebuilt=rebuilt_count, failed=failed)

    # ------------------------------------------------------------------
    # Internal helpers

    def _compute_rebuild_order(self, packages_to_rebuild: Set[str]) -> List[str]:
        formulae = self._get_formulae_info(False)
        graph = nx.DiGraph()

        source_built = set()
        for formula in formulae.values():
            installed = formula.get("installed") or []
            if installed and not installed[0].get("built_as_bottle"):
                pkg_name = formula.get("name")
                if pkg_name:
                    source_built.add(pkg_name)
                    graph.add_node(pkg_name)

        for formula in formulae.values():
            installed = formula.get("installed") or []
            if not installed or installed[0].get("built_as_bottle"):
                continue
            pkg_name = formula.get("name")
            if not pkg_name or pkg_name not in source_built:
                continue
            for dep in installed[0].get("runtime_dependencies", []):
                dep_name = dep.get("full_name")
                if dep_name in source_built:
                    graph.add_edge(pkg_name, dep_name)

        try:
            cycles = list(nx.simple_cycles(graph))
            if cycles:
                log.warning("Circular dependencies detected:")
                for cycle in cycles[:3]:
                    log.warning(f"    {' -> '.join(cycle + [cycle[0]])}")
                if len(cycles) > 3:
                    log.warning(f"    ... and {len(cycles) - 3} more cycles")
                log.info("  Breaking cycles by ignoring back-edges...")
                graph = nx.DiGraph(nx.dag.transitive_reduction(nx.DiGraph(graph)))
        except nx.NetworkXException:
            pass

        packages_affected = set(packages_to_rebuild)
        for pkg in list(packages_to_rebuild):
            predecessors = nx.ancestors(graph, pkg) if pkg in graph else set()
            packages_affected.update(predecessors)

        try:
            order = list(nx.topological_sort(graph.subgraph(packages_affected)))
            order.reverse()
            return order
        except nx.NetworkXUnfeasible:
            log.warning("Could not determine dependency order, using original order")
            return list(packages_affected)

    def _detect_running_services(self, rebuild_order: List[str]) -> Dict[str, str]:
        try:
            from subprocess import run

            result = run(["brew", "services", "list"], capture_output=True, text=True, check=True)
        except Exception:
            return {}

        services: Dict[str, str] = {}
        lines = result.stdout.strip().splitlines()[1:]
        for line in lines:
            parts = line.split()
            if len(parts) >= 2:
                services[parts[0]] = parts[1]

        active = {}
        for pkg_name in rebuild_order:
            status = services.get(pkg_name)
            if status in {"started", "running"}:
                active[pkg_name] = status
                log.info(f"  Found running service: {pkg_name}")
        return active


def subprocess_run(cmd):  # pragma: no cover - thin wrapper for mocking if needed
    import subprocess

    return subprocess.run(cmd, check=True, capture_output=True)
