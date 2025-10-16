# Global Codex CLI Guidance

- Prefer using `apply_patch` for file edits to keep changes safe and reviewable.
  - `apply_patch` provides an easier to review output to the user than any other tool, and is also easier to give permissions to.
  - Despite the name, it is also fully capable of creating files.
  - Prefer `apply_patch` ALWAYS against python/cat/bash/etcetera
- When a command fails due to sandbox-related issues (permission denied, blocked network, socket errors, etc.), retry with elevated permissions to request the user’s approval to bypass the sandbox.
  - This also applies to `apply_patch`, as it can run into sandbox permission issues.
  - In general, most errors you run into will likely be sandbox-related, when trying to call shell commands.
  - Every single time you see `failed in sandbox`, retry again the very same query, exactly as-is, while asking for permissions. That message always means "ask the user if I can run it".
- When running a Python command that requires a missing dependency, switch to `uv run --with package1 package2 -- {script}` so it can be pulled at runtime.
- The current environment uses mise to maintain tooling versions; this means:
  - If due to some misconfiguration a command fails, running it as `mise exec -- {command}` may work if it is managed by Mise.
  - If setting up a new project, adding a mise.toml file to the root and then using `mise add` and `mise install` simplifies pinning tooling versions.
  - You may use a combination of `mise ls-remote {tool}` and "raw" `mise registry` to see what's available; `mise ls` to see what's locally installed/configured.
- `run_shell` does NOT need you to wrap your command into a shell call; that is, `echo SOMETHING` will be the same as `zsh -cl 'echo SOMETHING`
  - As with `apply_patch`, calling it "simpler" without wrapping the command in an explicit shell call will be easier for the user to review and give wider permissions to when running into sandbox issues.
- When both chrome-devtools and Playwright MCP servers are present, favor Playwright for building or regression-checking flows, and switch to chrome-devtools when you need Chrome-specific console/network/perf insight for bug hunts.
- Browser sessions from different MCP servers are isolated; you cannot hop from one server to drive or inspect the other's state.
