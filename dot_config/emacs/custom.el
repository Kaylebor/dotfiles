;; -*- lexical-binding: t; -*-
;; Enable minibuffer history
(savehist-mode 1)

;; Makes manual buffer switching obey configured display actions
(setopt switch-to-buffer-obey-display-actions t)

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)                 ; Set a minimum width

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Set tabs to spaces
(setq-default indent-tabs-mode nil)
;; Set tab to two spaces
(setq-default tab-width 2)
(setq-default smie-indent-basic 2)
(setq-default c-basic-offset 2)
(setq-default sh-basic-offset 2)
(setq-default standard-indent 2)
(setq-default typescript-indent-level 2)
(setq-default evil-shift-width 2)

;; Force Emacs to use a POSIX shell
(setopt shell-file-name (executable-find "bash"))
;; Switch vterm to fish shell
(setopt vterm-shell (executable-find "fish"))
(setopt explicit-shell-file-name (executable-find "fish"))

;; Magit config
(setopt magit-define-global-key-bindings 'recommended)

;; 1Password config
(setopt auth-source-1password-vault "Private")
(setopt auth-source-1password-construct-secret-reference #'auth-source-1password--1password-construct-query-path-escaped)
