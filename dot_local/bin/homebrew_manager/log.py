"""Simple logging helpers for Homebrew manager scripts."""

from __future__ import annotations

from typing import Any


def info(message: str, *, prefix: str | None = None) -> None:
    if prefix:
        print(f"{prefix}{message}")
    else:
        print(message)


def success(message: str) -> None:
    info(message, prefix="✅ ")


def warning(message: str) -> None:
    info(message, prefix="⚠️  ")


def error(message: str) -> None:
    info(message, prefix="❌ ")


def step(message: str) -> None:
    info(message)
