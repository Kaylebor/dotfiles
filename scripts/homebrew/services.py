"""Utilities for interacting with Homebrew services."""

from __future__ import annotations

from typing import Dict, Iterable

from . import log

try:
    from subprocess import run
except ImportError:  # pragma: no cover - defensive
    run = None  # type: ignore


def list_services() -> Dict[str, str]:
    """Return mapping of service name -> status using `brew services list`."""
    if run is None:  # pragma: no cover
        return {}
    try:
        result = run(["brew", "services", "list"], capture_output=True, text=True, check=True)
    except Exception as exc:  # pragma: no cover - defensive
        log.warning(f"Unable to read brew services list: {exc}")
        return {}

    services: Dict[str, str] = {}
    lines = result.stdout.strip().splitlines()[1:]
    for line in lines:
        parts = line.split()
        if len(parts) >= 2:
            services[parts[0]] = parts[1]
    return services


def stop_service(name: str) -> bool:
    if run is None:  # pragma: no cover
        return False
    try:
        log.info(f"  Stopping service: {name}")
        run(["brew", "services", "stop", name], check=True, capture_output=True)
        return True
    except Exception as exc:  # pragma: no cover
        log.warning(f"  Failed to stop service {name}: {exc}")
        return False


def start_service(name: str) -> bool:
    if run is None:  # pragma: no cover
        return False
    try:
        log.info(f"  Starting service: {name}")
        run(["brew", "services", "start", name], check=True, capture_output=True)
        return True
    except Exception as exc:  # pragma: no cover
        log.warning(f"  Failed to start service {name}: {exc}")
        return False


def restart_service(name: str) -> bool:
    if run is None:  # pragma: no cover
        return False
    try:
        log.info(f"  Restarting service: {name}")
        run(["brew", "services", "restart", name], check=True, capture_output=True)
        return True
    except Exception as exc:  # pragma: no cover
        log.warning(f"  Failed to restart service {name}: {exc}")
        return False


def active_services(names: Iterable[str]) -> Dict[str, str]:
    services = list_services()
    result: Dict[str, str] = {}
    for name in names:
        status = services.get(name)
        if status in {"started", "running"}:
            result[name] = status
            log.info(f"  Found running service: {name}")
    return result
