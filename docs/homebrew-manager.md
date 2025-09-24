# Homebrew Manager

The Homebrew manager scripts in this repository provide a consistent way to install and maintain packages across both standard macOS setups and MDM-restricted environments where Homebrew lives in `~/.homebrew`. The system is split into a lightweight shell entry point (required by Chezmoi) and a richer Python CLI that performs the real work.

## Runtime Flow

1. **Chezmoi wrappers** (`run_onchange_before_install-packages.bash.tmpl` and related `run_once` scripts) build a JSON config describing the current machine (installation type, paths, URLs, etc.).
2. The wrapper invokes `~/.local/bin/chezmoi-homebrew-manager` with the `full-install` subcommand and passes that JSON.
3. The Python CLI installs/updates Homebrew, applies patches, installs taps/packages, triggers rebuilds, and runs post-install tasks such as kitty terminfo and Spotlight indexing.
4. Additional subcommands (`check-outdated`, `rebuild-outdated`, `mise-refresh`, etc.) support ad-hoc maintenance tasks.

## Modules

The CLI is organised into small modules under `dot_local/bin/homebrew_manager/`:

- **`config.PackagesHelper`** loads `.chezmoidata/packages.yml` and exposes helpers for install order, per-package metadata, env/args flags, and lists of brews/casks/taps.
- **`install.InstallService`** handles Homebrew bootstrap, tap installation, package installs (individual and bundle), missing-package detection, kitty terminfo, Spotlight reindexing, and a `mise-refresh` helper for reinstalling managed tools (currently ruby, with interactive prompts).
- **`rebuild.RebuildService`** encapsulates dependency detection (`brew info --json=v2 --installed`), networkx-based rebuild ordering, and service stop/start. It takes a callback for reinstalling packages so it works cleanly with `InstallService`.
- **`log`** provides simple `info/success/warning/error/step` helpers for consistent output.
- **`services`** wraps `brew services` start/stop/list logic so other tooling can reuse it (currently used by the rebuild service).

## CLI Overview

| Command | Description & Notes |
| --- | --- |
| `full-install [--config JSON] [--first-run] [--dry-run]` | Orchestrates Homebrew install/update, tap setup, package installs, rebuild detection, kitty terminfo, Spotlight indexing. Exit code `0` on success, `1` on failure. Dry-run short-circuits before executing actions. |
| `force-reinstall <pkg,…>` | Uninstalls + reinstalls packages using metadata from `.chezmoidata/packages.yml`. Validates names, sorts by install order. Dry-run prints the planned work. |
| `simple-reinstall <pkg,…>` | Runs `brew reinstall` for the provided formulae; mirrors the legacy `CHEZMOI_REINSTALL_PACKAGES` behaviour. |
| `check-outdated` | Detects source-built formulae with newer dependencies. Returns `0` when nothing needs rebuild, `1` otherwise. Dry-run just prints a message. |
| `rebuild-outdated` | Rebuilds packages identified by the detection step, stopping/starting relevant services. Returns `0` when everything succeeds, `1` if any rebuild fails. |
| `mise-refresh [--tool ruby] [--yes] [--dry-run]` | Lists installed Ruby versions and prompts before reinstalling each (optional `--yes` auto-confirms). Runs `mise reshim` after successful reinstalls. |

The interactive `brew-check-outdated` wrapper simply calls `check-outdated` and, if needed, asks whether to run `rebuild-outdated`.

## Why the Two-Layer Design?

Chezmoi run hooks must be shell scripts, so the wrapper handles templating and environment setup. Everything else lives in Python where it’s easier to manage complex workflows, reuse code, and expose additional subcommands without bloating the shell templates.

## Maintenance Notes

- The CLI accepts `--dry-run` for all subcommands, making it safe to preview actions during `chezmoi apply --dry-run` or ad-hoc runs.
- Run hooks now call `mise-refresh --tool ruby`, which prompts before rebuilding each installed Ruby version (including inactive ones). Use `--yes` to bypass prompts in scripted contexts.
- Git and Chezmoi ignore helper executables (e.g., `~/.local/bin/chezmoi-homebrew-manager`) and Python bytecode so the helpers run from the source directory without being copied into `$HOME`.
