# Global Codex CLI Guidance

- Prefer using `apply_patch` for file edits to keep changes safe and reviewable.
- When a command fails due to sandbox-related issues (permission denied, blocked network, socket errors, etc.), retry with elevated permissions to request the user’s approval to bypass the sandbox.
