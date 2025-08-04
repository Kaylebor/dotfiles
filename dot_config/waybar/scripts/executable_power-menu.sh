#!/bin/bash

# Power menu using fuzzel
options="  Lock
  Logout
󰒲  Suspend
  Reboot
  Shutdown"

# Show menu positioned near the top-right corner
# Adjust X position to be safely within screen bounds
chosen=$(echo -e "$options" | fuzzel --dmenu \
    --prompt "Power Menu: " \
    --lines 5 \
    --width 15 \
    --anchor top-right \
    --x-margin 20 \
    --y-margin 50)

case "$chosen" in
    "  Lock")
        hyprlock
        ;;
    "  Logout")
        hyprctl dispatch exit
        ;;
    "󰒲  Suspend")
        systemctl suspend
        ;;
    "  Reboot")
        systemctl reboot
        ;;
    "  Shutdown")
        systemctl poweroff
        ;;
esac