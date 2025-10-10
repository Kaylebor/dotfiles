# Global Codex CLI Guidance

- Prefer using `apply_patch` for file edits to keep changes safe and reviewable.
- When a command fails due to sandbox-related issues (permission denied, blocked network, socket errors, etc.), retry with elevated permissions to request the user’s approval to bypass the sandbox.
- When running a Python command that requires a missing dependency, switch to `uv run --with package1 package2 -- {script}` so it can be pulled at runtime.
- The current environment uses mise to maintain tooling versions; this means:
  - If due to some misconfiguration a command fails, running it as `mise exec -- {command}` may work if it is managed by Mise.
  - If setting up a new project, adding a mise.toml file to the root and then using `mise add` and `mise install` simplifies pinning tooling versions.
  - You may use a combination of `mise ls-remote {tool}` and "raw" `mise registry` to see what's available; `mise ls` to see what's locally installed/configured.
