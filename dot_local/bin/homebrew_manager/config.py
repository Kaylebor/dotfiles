"""Configuration helpers for Homebrew manager scripts."""

from __future__ import annotations

from dataclasses import dataclass
from math import inf
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional

import yaml


def _safe_load_yaml(path: Path) -> Dict[str, Any]:
    if not path.exists():
        return {}
    try:
        with path.open("r", encoding="utf-8") as handle:
            return yaml.safe_load(handle) or {}
    except Exception as exc:  # pragma: no cover - defensive
        print(f"Error loading packages config {path}: {exc}")
        return {}


@dataclass
class PackagesHelper:
    """Convenience accessors for packages.yml content."""

    data: Dict[str, Any]
    is_alternative: bool = False

    @classmethod
    def load(cls, path: Path, *, is_alternative: bool = False) -> "PackagesHelper":
        return cls(_safe_load_yaml(path), is_alternative=is_alternative)

    # ------ raw sections -------------------------------------------------

    @property
    def formula_patches(self) -> Dict[str, Any]:
        return self.data.get("formula_patches", {}) or {}

    @property
    def brews(self) -> List[Any]:  # entries can be str or dict
        return (self.data.get("packages", {}) or {}).get("darwin", {}).get("brews", []) or []

    @property
    def casks(self) -> List[Any]:
        return (self.data.get("packages", {}) or {}).get("darwin", {}).get("casks", []) or []

    @property
    def taps(self) -> List[str]:
        return (self.data.get("packages", {}) or {}).get("darwin", {}).get("taps", []) or []

    # ------ lookup helpers ----------------------------------------------

    def get_entry(self, name: str) -> Optional[Dict[str, Any]]:
        for entry in self.brews:
            normalized = self._normalize_entry(entry)
            if normalized and normalized.get("name") == name:
                return normalized
        return None

    def iter_entries(self) -> Iterable[Dict[str, Any]]:
        for entry in self.brews:
            normalized = self._normalize_entry(entry)
            if normalized:
                yield normalized

    def install_order(self, name: str) -> float:
        entry = self.get_entry(name)
        if not entry:
            return inf

        # Top-level order takes precedence
        if "install_order" in entry:
            try:
                return float(entry["install_order"])
            except Exception:
                return inf

        # Alternative-specific override
        if self.is_alternative:
            alt = entry.get("alternative_only") or {}
            if isinstance(alt, dict) and "install_order" in alt:
                try:
                    return float(alt["install_order"])
                except Exception:
                    return inf

        return inf

    def force_reinstall_enabled(self, name: str) -> bool:
        entry = self.get_entry(name)
        if not entry:
            return False

        flag = bool(entry.get("force_reinstall", False))
        if self.is_alternative:
            alt = entry.get("alternative_only") or {}
            if isinstance(alt, dict):
                flag = bool(alt.get("force_reinstall", flag))
        return flag

    # ------ internal utilities -----------------------------------------

    @staticmethod
    def _normalize_entry(entry: Any) -> Optional[Dict[str, Any]]:
        if isinstance(entry, dict):
            return entry
        if isinstance(entry, str):
            return {"name": entry, "type": "string"}
        return None

