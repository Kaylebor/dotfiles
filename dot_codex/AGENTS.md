# Global Codex CLI Guidance

- Prefer using `apply_patch` for file edits to keep changes safe and reviewable.
- When a command fails due to sandbox-related issues (permission denied, blocked network, socket errors, etc.), retry with elevated permissions to request the user’s approval to bypass the sandbox.
- When running a Python command that requires a missing dependency, switch to `uv run --with package1 package2 -- {script}` so it can be pulled at runtime.
