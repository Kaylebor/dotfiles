;; -*- lexical-binding: t; -*-
;; Mode-agnostic keybindings

;; Enable windmove for Shift+arrow window navigation
(windmove-default-keybindings)

;; Keybinds for completion-preview
(with-eval-after-load 'completion-preview
  ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;; Org file navigation
(global-set-key (kbd "C-c o f") #'my/consult-org-files)

;; Claude Code IDE vterm integration
;; Make C-c C-c send escape to Claude (for canceling operations)
;; This overrides the default vterm C-c C-c (which sends C-c)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-c") #'claude-code-ide-send-escape))