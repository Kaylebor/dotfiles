;; -*- lexical-binding: t; -*-
;; Window Management Configuration
;; Consistent buffer placement using display-buffer-alist

(setopt display-buffer-alist
  `(
    ;; Vterm buffers - dedicated bottom window
    ("\\*vterm.*\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)
     (window-height . 0.3)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ;; Help and documentation buffers - right side
    ("\\*Help\\*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 1)
     (window-width . 0.4)
     (dedicated . t)
     (preserve-size . (t . nil)))
    
    ("\\*helpful .*\\*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 1)
     (window-width . 0.4)
     (dedicated . t)
     (preserve-size . (t . nil)))
    
    ("\\*info\\*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 1)
     (window-width . 0.4)
     (dedicated . t)
     (preserve-size . (t . nil)))
    
    ;; Compilation and output buffers - bottom window
    ("\\*compilation\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)
     (window-height . 0.25)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ("\\*grep\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)
     (window-height . 0.25)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ("\\*rg\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)
     (window-height . 0.25)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ;; Shell buffers - bottom window (except vterm which is handled above)
    ("\\*shell\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 2)
     (window-height . 0.3)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ("\\*eshell\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 2)
     (window-height . 0.3)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ;; Test output buffers - bottom window
    ("\\*pytest\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)
     (window-height . 0.25)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ("\\*cargo-test\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)
     (window-height . 0.25)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ;; LSP/Eglot diagnostics and references - right side
    ("\\*xref\\*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 2)
     (window-width . 0.4)
     (dedicated . t)
     (preserve-size . (t . nil)))
    
    ("\\*Flymake diagnostics.*\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 3)
     (window-height . 0.25)
     (dedicated . t)
     (preserve-size . (nil . t)))
    
    ;; Claude Code IDE - already configured in init.el but ensure consistency
    ("\\*claude-code-ide.*\\*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . -1)  ; Negative slot to appear first
     (window-width . 0.5)
     (dedicated . t)
     (preserve-size . (t . nil)))
    
    ;; Org Agenda - right side
    ("\\*Org Agenda\\*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 1)
     (window-width . 0.4)
     (dedicated . t)
     (preserve-size . (t . nil)))
    
    ;; Backup and emergency buffers - use main window area for special cases
    ("\\*scratch\\*"
     (display-buffer-reuse-window display-buffer-same-window))
    
    ("\\*Messages\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 4)
     (window-height . 0.2)
     (dedicated . t)
     (preserve-size . (nil . t)))
    ))

