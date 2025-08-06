;; -*- lexical-binding: t; -*-
(setopt evil-want-keybinding nil)

(use-package hydra :ensure t)

(use-package undo-tree :ensure t
  :delight
  :init
  (global-undo-tree-mode))

(use-package evil :ensure t
  :delight
  :custom
  (evil-want-integration t)
  (evil-undo-system 'undo-tree)
  :init
  (evil-mode 1))

(use-package evil-collection :ensure t
  :delight evil-collection-unimpaired-mode
  :init
  (evil-collection-init))

(with-eval-after-load 'evil
  ;; Replace M-: with pp-eval-expression
  (keymap-set evil-normal-state-map "M-:" #'pp-eval-expression)
  (keymap-set evil-normal-state-map "z a" #'treesit-fold-toggle)
  (keymap-set evil-normal-state-map "g l g" #'aidermacs-transient-menu)
  (keymap-set evil-normal-state-map "g l l" #'gptel-menu)
  (keymap-set evil-normal-state-map "g l s" #'gptel-send)
  (keymap-set evil-normal-state-map "g l a" #'gptel-add)
  (keymap-set evil-normal-state-map "g l f" #'gptel-add-file)
  (keymap-set evil-normal-state-map "M-?" #'which-key-show-top-level)
  
  ;; Claude Code IDE bindings
  (keymap-set evil-normal-state-map "g l c" #'claude-code-ide-menu)
  (keymap-set evil-normal-state-map "g l C" #'claude-code-ide)
  (keymap-set evil-normal-state-map "g l r" #'claude-code-ide-resume)
  (keymap-set evil-normal-state-map "g l b" #'claude-code-ide-switch-to-buffer)
  (keymap-set evil-normal-state-map "g l q" #'claude-code-ide-stop)
  (keymap-set evil-normal-state-map "g l w" #'claude-code-ide-toggle-window)
  
  ;; Visual mode binding for sending selection to Claude
  (keymap-set evil-visual-state-map "g l i" #'claude-code-ide-insert-at-mentioned))

;; Keybinds for completion-preview
(with-eval-after-load 'completion-preview
  ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;; Claude Code IDE evil-mode vterm integration
;; Make C-c C-c send escape to Claude (for canceling operations)
;; This overrides the default vterm C-c C-c (which sends C-c)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-c") #'claude-code-ide-send-escape))
