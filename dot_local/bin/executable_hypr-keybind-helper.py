#!/usr/bin/env python3

import os
import re
import subprocess
from pathlib import Path

# Keybind descriptions
DESCRIPTIONS = {
    # Terminal and core
    "Return, exec, $terminal": "Open terminal",
    "C, killactive,": "Close window",
    "M, exit,": "Exit Hyprland",
    "E, exec, $fileManager": "Open file manager",
    "SHIFT, E, exec, emacsclient -c": "Open Emacs client",
    "V, togglefloating,": "Toggle floating mode",
    "Space, exec, $menu": "Open app launcher",
    "P, pseudo,": "Toggle pseudo-tiling",
    "J, togglesplit,": "Toggle split direction",
    
    # Focus movement
    "left, movefocus, l": "Focus left window",
    "right, movefocus, r": "Focus right window",
    "up, movefocus, u": "Focus upper window",
    "down, movefocus, d": "Focus lower window",
    
    # Window resizing
    "ALT, left, resizeactive, -50 0": "Resize window smaller horizontally",
    "ALT, right, resizeactive, 50 0": "Resize window larger horizontally",
    "ALT, up, resizeactive, 0 -50": "Resize window smaller vertically",
    "ALT, down, resizeactive, 0 50": "Resize window larger vertically",
    
    # Window movement
    "SHIFT, left, movewindow, l": "Move window left",
    "SHIFT, right, movewindow, r": "Move window right",
    "SHIFT, up, movewindow, u": "Move window up",
    "SHIFT, down, movewindow, d": "Move window down",
    
    # Window swapping
    "CTRL, left, swapwindow, l": "Swap with left window",
    "CTRL, right, swapwindow, r": "Swap with right window",
    "CTRL, up, swapwindow, u": "Swap with upper window",
    "CTRL, down, swapwindow, d": "Swap with lower window",
    
    # Workspaces
    "1, workspace, 1": "Switch to workspace 1",
    "2, workspace, 2": "Switch to workspace 2",
    "3, workspace, 3": "Switch to workspace 3",
    "4, workspace, 4": "Switch to workspace 4",
    "5, workspace, 5": "Switch to workspace 5",
    "6, workspace, 6": "Switch to workspace 6",
    "7, workspace, 7": "Switch to workspace 7",
    "8, workspace, 8": "Switch to workspace 8",
    "9, workspace, 9": "Switch to workspace 9",
    "0, workspace, 10": "Switch to workspace 10",
    
    # Move window to workspace
    "SHIFT, 1, movetoworkspace, 1": "Move window to workspace 1",
    "SHIFT, 2, movetoworkspace, 2": "Move window to workspace 2",
    "SHIFT, 3, movetoworkspace, 3": "Move window to workspace 3",
    "SHIFT, 4, movetoworkspace, 4": "Move window to workspace 4",
    "SHIFT, 5, movetoworkspace, 5": "Move window to workspace 5",
    "SHIFT, 6, movetoworkspace, 6": "Move window to workspace 6",
    "SHIFT, 7, movetoworkspace, 7": "Move window to workspace 7",
    "SHIFT, 8, movetoworkspace, 8": "Move window to workspace 8",
    "SHIFT, 9, movetoworkspace, 9": "Move window to workspace 9",
    "SHIFT, 0, movetoworkspace, 10": "Move window to workspace 10",
    
    # Special workspace
    "S, togglespecialworkspace, magic": "Toggle scratchpad",
    "SHIFT, S, movetoworkspace, special:magic": "Move window to scratchpad",
    
    # Mouse workspaces
    ", mouse_down, workspace, e+1": "Next workspace",
    ", mouse_up, workspace, e-1": "Previous workspace",
    
    # Screenshots
    ", Print, exec, $screenshot copy area": "Screenshot area",
    "SHIFT, Print, exec, $screenshot copy screen": "Screenshot full screen",
    "SHIFT, Print, exec, $screenshot copy output": "Screenshot current monitor",
    
    # System
    "L, exec, $screenlock": "Lock screen",
    "F1, exec, $keybindhelper": "Show this keybind help",
    
    # Applications
    "CTRL SHIFT, Space, exec, 1password --quick-access": "1Password Quick Access",
    
    # Mouse controls
    "mouse:272, movewindow": "Move window with mouse",
    "mouse:273, resizewindow": "Resize window with mouse",
    
    # Media keys
    ",XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+": "Volume up",
    ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-": "Volume down",
    ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle": "Toggle mute",
    ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle": "Toggle mic mute",
    ",XF86MonBrightnessUp, exec, brightnessctl -e4 -n2 set 5%+": "Brightness up",
    ",XF86MonBrightnessDown, exec, brightnessctl -e4 -n2 set 5%-": "Brightness down",
    ", XF86AudioNext, exec, playerctl next": "Next track",
    ", XF86AudioPause, exec, playerctl play-pause": "Play/Pause",
    ", XF86AudioPlay, exec, playerctl play-pause": "Play/Pause",
    ", XF86AudioPrev, exec, playerctl previous": "Previous track",
}

# Symbol replacements
SYMBOL_MAP = {
    "$mainMod": "⌘",
    "SHIFT": "⇧",
    "CTRL": "⌃",
    "ALT": "⌥",
    "Return": "↵",
    "Space": "␣",
    "Print": "PrtSc",
    "left": "←",
    "right": "→",
    "up": "↑",
    "down": "↓",
    # Media keys
    "XF86AudioRaiseVolume": "♪↑",
    "XF86AudioLowerVolume": "♪↓",
    "XF86AudioMute": "♪⊘",
    "XF86AudioMicMute": "⎙⊘",
    "XF86MonBrightnessUp": "☼",
    "XF86MonBrightnessDown": "☾",
    "XF86AudioNext": "⏭",
    "XF86AudioPause": "⏸",
    "XF86AudioPlay": "⏵",
    "XF86AudioPrev": "⏮",
    # Mouse
    "mouse:272": "LMB",
    "mouse:273": "RMB",
    "mouse_down": "ScrollDown",
    "mouse_up": "ScrollUp",
}

def parse_hyprland_config():
    """Parse hyprland config and extract keybinds."""
    config_path = Path.home() / ".config/hypr/hyprland.conf"
    keybinds = []
    
    # Get mainMod value
    main_mod = "SUPER"
    with open(config_path, 'r') as f:
        for line in f:
            if line.startswith('$mainMod'):
                main_mod = line.split('=')[1].strip()
                break
    
    # Parse keybinds
    with open(config_path, 'r') as f:
        for line in f:
            match = re.match(r'^bind[elmr]*\s*=\s*(.+)$', line)
            if match:
                bind_content = match.group(1)
                
                # Extract key combo (everything before the action)
                parts = bind_content.split(',', 2)
                if len(parts) >= 2:
                    key_combo = ','.join(parts[:2])
                else:
                    key_combo = bind_content
                
                # Find description
                description = "(No description)"
                for pattern, desc in DESCRIPTIONS.items():
                    # Escape regex special chars
                    escaped_pattern = re.escape(pattern)
                    if re.search(rf'\$mainMod\s*{escaped_pattern}|{escaped_pattern}', bind_content):
                        description = desc
                        break
                
                keybinds.append((key_combo, description))
    
    return keybinds, main_mod

def format_keybind(key, main_mod):
    """Format keybind with Unicode symbols."""
    # Replace mainMod first
    key = key.replace("$mainMod", SYMBOL_MAP.get("$mainMod", main_mod))
    
    # Replace longer strings first to avoid partial replacements
    # Sort by length in descending order
    sorted_items = sorted(SYMBOL_MAP.items(), key=lambda x: len(x[0]), reverse=True)
    
    for old, new in sorted_items:
        if old != "$mainMod":  # Already handled
            key = key.replace(old, new)
    
    # Handle comma to plus conversion
    if key.startswith(","):
        # Standalone key (no modifier)
        key = key[1:].strip()
    else:
        # Convert commas to plus signs
        key = key.replace(", ", " + ")
    
    # Clean up
    key = re.sub(r'\s+', ' ', key).strip()
    
    return key

def main():
    """Main function to display keybinds."""
    keybinds, main_mod = parse_hyprland_config()
    
    # Format all keybinds and find max width
    formatted_binds = []
    max_width = 0
    
    for key_combo, description in keybinds:
        formatted_key = format_keybind(key_combo, main_mod)
        formatted_binds.append((formatted_key, description))
        # Use wcwidth for proper Unicode width calculation
        # For now, we'll use a fixed tab width approach
    
    # Output formatted keybinds
    output_lines = []
    for key, desc in formatted_binds:
        # Pad key to ensure minimum spacing
        padded_key = key.ljust(25)
        output_lines.append(f"{padded_key}{desc}")
    
    # Send to fuzzel
    process = subprocess.Popen(
        ['fuzzel', '--dmenu', '--prompt', 'Keybinds: ', '--width', '75', 
         '--lines', '25', '--font', 'JetBrainsMono Nerd Font:size=10'],
        stdin=subprocess.PIPE,
        text=True
    )
    process.communicate('\n'.join(output_lines))

if __name__ == "__main__":
    main()