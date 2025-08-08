#!/bin/bash
# Toggle systemd idle inhibitor for hypridle compatibility

INHIBIT_FILE="/tmp/idle-inhibit-$USER.lock"
INHIBIT_PID_FILE="/tmp/idle-inhibit-$USER.pid"

if [ -f "$INHIBIT_PID_FILE" ]; then
    # Inhibitor is active, kill entire process group
    PID=$(cat "$INHIBIT_PID_FILE")
    # Kill the process group (negative PID)
    kill -- -$PID 2>/dev/null || kill $PID 2>/dev/null
    # Also kill any remaining systemd-inhibit processes with our reason
    pkill -f "systemd-inhibit.*Manual idle inhibit via Waybar" 2>/dev/null
    rm -f "$INHIBIT_PID_FILE" "$INHIBIT_FILE"
    echo '{"text": "", "class": "deactivated", "tooltip": "Idle inhibitor deactivated"}'
else
    # Start inhibitor in background (inhibit idle, sleep, and handle-lid-switch)
    nohup systemd-inhibit --what=idle:sleep:handle-lid-switch --who="User" --why="Manual idle inhibit via Waybar" --mode=block sleep infinity > /dev/null 2>&1 &
    echo $! > "$INHIBIT_PID_FILE"
    touch "$INHIBIT_FILE"
    echo '{"text": "", "class": "activated", "tooltip": "Idle inhibitor activated"}'
fi