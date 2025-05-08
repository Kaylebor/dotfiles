;; -*- lexical-binding: t; -*-
;; Setup elpaca package manager https://github.com/progfolio/elpaca/blob/master/doc/manual.md#usage
(load-file (expand-file-name "elpaca-init.el" user-emacs-directory))

;; Personal functions; some may be used in configurations
(load-file (expand-file-name "functions.el" user-emacs-directory))

;; PACKAGES
;; Improves some MacOS defaults
(when (or (daemonp) (memq window-system '(mac ns))) (elpaca exec-path-from-shell (exec-path-from-shell-initialize)))

;; yasnippet
(use-package yasnippet :ensure t
  :init
  (yas-global-mode 1))

;; tree-sitter and eglot
(load-file (expand-file-name "syntax.el" user-emacs-directory))

;; transient
(use-package transient :ensure t)

;; ripgrep
(use-package rg :ensure t
  :init
  (rg-enable-menu))

;; projectile
(use-package projectile :ensure t
  :custom
  (projectile-keymap-prefix (kbd "C-x p"))
  (projectile-project-search-path '("~/projects/" "~/work/" "~/playground"))
  :config
  (projectile-mode))

;; dired-sidebar for visual navigation
(use-package dired-sidebar :ensure t
  :bind
  ("C-x t" . dired-sidebar-toggle-sidebar)
  :config
  (if (display-graphic-p)
      (setopt dired-sidebar-theme 'icons)
    (setopt dired-sidebar-theme 'nerd)))

;; completion-preview for minimal completion suggestions (built-in)
(global-completion-preview-mode)

;; corfu for improved completion UI
(use-package corfu :ensure t
  :init
  (global-corfu-mode))

;; vertico for improved completion UI https://github.com/minad/vertico
(use-package vertico :ensure t
  :config
  (vertico-mode +1)
  (vertico-mouse-mode))

;; consult provides a bunch of search commands to vertico https://github.com/minad/consult
(use-package consult
  :bind
  (:map consult-mode-map
    ("C-x f" . consult-find)
    ("C-x ." . consult-ripgrep)
    ("C-x C-b" . consult-buffer) ;; Show buffer list
    ("C-x p b" . consult-project-buffer)
    ("M-g g" . consult-goto-line)
    ("M-g i" . consult-imenu)
    )
  :custom
  (consult-project-function (lambda (_) (projectile-project-root)))
  :config
  (autoload 'projectile-project-root "projectile"))

;; Marginalia to add completion information https://github.com/minad/marginalia
(use-package marginalia :ensure t
  :config (marginalia-mode))

;; embark provides contextual actions to vertico https://github.com/oantolin/embark
(use-package embark :ensure t
  :custom
  (prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult :ensure t
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; orderless for improved completion matching https://github.com/oantolin/orderless
(use-package orderless :ensure t
  :custom
  (completion-styles '(partial-completion orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

;; eldoc-box shows documentation in a floating box
(use-package eldoc-box :ensure t
  :bind
  (("C-x K" . eldoc-box-help-at-point))
  :config
  (eldoc-box-hover-at-point-mode))

;; Turns grep searches into writable buffers https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep :ensure t)

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))

;; vterm
(use-package vterm :ensure t
  :bind
  (("C-c t" . vterm)))

;; magit
(use-package magit :ensure t)

;; which-key for command discovery
(use-package which-key :ensure t
  :init
  (which-key-mode))

;; Code folding based on newer treesit.el
(use-package treesit-fold :ensure t
  :bind
  (:map treesit-fold-mode-map
    ("C-c C-f" . treesit-fold-toggle)
    ("C-c C-u" . treesit-fold-unfold-all)
    ("C-c C-r" . treesit-fold-rebuild))
  :custom
  (treesit-fold-line-count-show t)
  :init
  (global-treesit-fold-mode)
  (global-treesit-fold-indicators-mode))

(use-package grip-mode
  :ensure t
  :custom
  (grip-command 'go-grip) ;; auto, grip, go-grip or mdopen
  :hook ((markdown-ts-mode org-mode) . grip-mode))

;; Better mise integration with Emacs
(use-package mise :ensure t
  :init
  (global-mise-mode))

;; DAP debugging
(use-package dape :ensure t)

;; 1Password integration
(elpaca (auth-source-1password :host github :repo "dlobraico/auth-source-1password" :build t)
  (auth-source-1password-enable))

;; GPT and family
(load-file (expand-file-name "gptel-init.el" user-emacs-directory))

;; icons
(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired :ensure t :hook (dired-sidebar-mode . all-the-icons-dired-mode))
;; Run all-the-icons-install-fonts afterwards

;; Emojis
(use-package emojify :ensure t
  :custom
  (emojify-emoji-styles '(unicode github))
  (emojify-display-style 'unicode)
  :init
  (global-emojify-mode))

;; Theme
(use-package dracula-theme :ensure t) ;; Needed, as catppuccin-theme depends on it
(use-package catppuccin-theme :ensure t
  :custom
  (catppuccin-flavor 'frappe)
  :init
  (load-theme 'catppuccin :no-confirm))

;; Keybindings go here
(load-file (expand-file-name "keybindings.el" user-emacs-directory))

;; Manually process elpaca queues now before loading customizations
;; Keybindings may go above this line if they depend on extra packages
(elpaca-process-queues)

;; Disable menu bar
(menu-bar-mode -1)

;; Allow for shorter responses: "y" for yes and "n" for no.
(setopt read-answer-short t)
(setopt use-short-answers t)
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; Allow nested minibuffers
(setopt enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setopt minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; By default, Emacs "updates" its ui more often than it needs to
(setopt idle-update-delay 1.0)

(setopt ring-bell-function #'ignore)

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setopt truncate-string-ellipsis "…")

;; Delete by moving to trash in interactive mode
(setopt delete-by-moving-to-trash (not noninteractive)
        remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Ignoring this is acceptable since it will redirect to the buffer regardless.
(setopt find-file-suppress-same-file-warnings t)

;; Resolve symlinks so that operations are conducted from the file's directory
(setopt find-file-visit-truename t
        vc-follow-symlinks t)

;; Prefer vertical splits over horizontal ones
(setopt split-width-threshold 170
        split-height-threshold nil)

;; Buffers
(setopt uniquify-buffer-name-style 'forward
        comint-prompt-read-only t
        comint-buffer-maximum-size 2048)

;; Backups
(setopt backup-directory-alist
        `(("." . ,(expand-file-name "backup" user-emacs-directory)))
        tramp-backup-directory-alist backup-directory-alist
        backup-by-copying-when-linked t
        backup-by-copying t
        delete-old-versions t
        version-control t
        kept-new-versions 5
        kept-old-versions 5)

;; VC
(setopt vc-git-print-log-follow t
        vc-make-backup-files nil)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.
(setopt revert-without-query (list ".")  ; Do not prompt
        auto-revert-stop-on-user-input nil
        auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setopt global-auto-revert-non-file-buffers t
        global-auto-revert-ignore-modes '(Buffer-menu-mode)) 

;; Enables Emacs to remember a list of recently accessed files.
(setopt recentf-max-saved-items 300 ; default is 20
        recentf-max-menu-items 15
        recentf-auto-cleanup (if (daemonp) 300 'never)
        recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))

;; Enables Emacs to remember the last location within a file upon reopening.
(setopt save-place-file (expand-file-name "saveplace" user-emacs-directory)
        save-place-limit 600)

;; Enables Emacs to preserve the minibuffer history between sessions.
(setopt history-length 300
        savehist-save-minibuffer-history t ;; Default
        savehist-additional-variables
        '(kill-ring                        ; clipboard
          register-alist                   ; macros
          mark-ring global-mark-ring       ; marks
          search-ring regexp-search-ring)) ; searches

;;; Scrolling
(setopt fast-but-imprecise-scrolling t
        scroll-error-top-bottom t
        scroll-preserve-screen-position t
        scroll-margin 0
        hscroll-margin 2
        hscroll-step 1)

;;; Mouse
(when (and (display-graphic-p) (fboundp 'context-menu-mode))
    (add-hook 'after-init-hook #'context-menu-mode))

;; Customization
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
