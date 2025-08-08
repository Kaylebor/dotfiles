#!/bin/bash
# Get systemd idle inhibitor status for Waybar

INHIBIT_PID_FILE="/tmp/idle-inhibit-$USER.pid"

if [ -f "$INHIBIT_PID_FILE" ]; then
    PID=$(cat "$INHIBIT_PID_FILE")
    if kill -0 $PID 2>/dev/null; then
        echo '{"text": "", "class": "activated", "tooltip": "Idle inhibitor activated"}'
    else
        # PID is dead, clean up
        rm -f "$INHIBIT_PID_FILE" "/tmp/idle-inhibit-$USER.lock"
        echo '{"text": "", "class": "deactivated", "tooltip": "Idle inhibitor deactivated"}'
    fi
else
    echo '{"text": "", "class": "deactivated", "tooltip": "Idle inhibitor deactivated"}'
fi