function docker
    # Resolve external binaries to avoid recursion through functions/aliases
    set -l docker_path (command -s docker ^/dev/null)
    if test -n "$docker_path"
        "$docker_path" $argv
        return $status
    end

    set -l nerdctl_path (command -s nerdctl ^/dev/null)
    if test -n "$nerdctl_path"
        "$nerdctl_path" $argv
        return $status
    end

    set -l lima_path (command -s lima ^/dev/null)
    if test -n "$lima_path"
        if "$lima_path" nerdctl version >/dev/null 2>&1
            "$lima_path" nerdctl $argv
            return $status
        end
    end

    echo "No container runtime found (docker, nerdctl, or lima nerdctl)." >&2
    return 1
end
