;; -*- lexical-binding: t; -*-

(use-package evil :ensure t
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  :init
  (evil-mode 1)
)

(use-package evil-collection :ensure t
  :init
  (evil-collection-init)
)

(with-eval-after-load 'evil
  ;; Replace M-: with pp-eval-expression
  (keymap-set evil-normal-state-map "M-:" #'pp-eval-expression)
  (keymap-set evil-normal-state-map "z a" #'treesit-fold-toggle)
)

;; Keybinds for completion-preview
(with-eval-after-load 'completion-preview
  ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert)
)
