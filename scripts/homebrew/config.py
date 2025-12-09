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
    except Exception as exc:
        print(f"Error loading packages config {path}: {exc}")
        return {}


@dataclass
class PackagesHelper:
    """Convenience accessors for packages.yml content."""

    data: Dict[str, Any]

    @classmethod
    def load(cls, path: Path) -> "PackagesHelper":
        return cls(_safe_load_yaml(path))

    # ------ raw sections -------------------------------------------------

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
        try:
            return float(entry.get("install_order", inf))
        except Exception:
            return inf

    # ------ entry helpers -------------------------------------------------

    @staticmethod
    def entry_envs(entry: Dict[str, Any]) -> Tuple[Dict[str, Any], Dict[str, Any]]:
        base = entry.get("env") if isinstance(entry, dict) else {}
        return (dict(base) if isinstance(base, dict) else {}, {})

    @staticmethod
    def entry_args(entry: Dict[str, Any]) -> List[str]:
        args: List[str] = []
        base_args = entry.get("args") if isinstance(entry, dict) else None
        if isinstance(base_args, list):
            args.extend(str(arg) for arg in base_args)
        return args

    def requires_individual(self, entry: Dict[str, Any]) -> bool:
        base_env, _ = self.entry_envs(entry)
        return bool(base_env)

    # ------ internal utilities -----------------------------------------

    @staticmethod
    def _normalize_entry(entry: Any) -> Optional[Dict[str, Any]]:
        if isinstance(entry, dict):
            return entry
        if isinstance(entry, str):
            return {"name": entry, "type": "string"}
        return None
