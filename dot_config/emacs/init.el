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
  (yas-global-mode 1)
)

;; tree-sitter and eglot
(load-file (expand-file-name "syntax.el" user-emacs-directory))

;; transient
(use-package transient :ensure t)

;; ripgrep
(use-package rg :ensure t
  :init
  (rg-enable-menu)
)

;; projectile
(use-package projectile :ensure t
  :custom
  (projectile-keymap-prefix (kbd "C-x p"))
  (projectile-project-search-path '("~/projects/" "~/work/" "~/playground"))
  :config
  (projectile-mode)
)

;; completion-preview for minimal completion suggestions (built-in)
(global-completion-preview-mode)

;; corfu for improved completion UI
(use-package corfu :ensure t
  :init
  (global-corfu-mode)
)

;; vertico for improved completion UI https://github.com/minad/vertico
(use-package vertico :ensure t
  :config
  (vertico-mode +1)
  (vertico-mouse-mode)
)

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
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
)

;; Marginalia to add completion information https://github.com/minad/marginalia
(use-package marginalia :ensure t
  :config (marginalia-mode)
)

;; embark provides contextual actions to vertico https://github.com/oantolin/embark
(use-package embark :ensure t
  :custom
  (prefix-help-command #'embark-prefix-help-command)
)
(use-package embark-consult :ensure t
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode)
)

;; orderless for improved completion matching https://github.com/oantolin/orderless
(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
)

;; eldoc-box shows documentation in a floating box
(use-package eldoc-box :ensure t
  :bind
  (("C-x K" . eldoc-box-help-at-point))
  :config
  (eldoc-box-hover-at-point-mode)
)

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
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
)

;; vterm
(use-package vterm :ensure t
  :bind
  (("C-c t" . vterm))
)

;; magit
(use-package magit :ensure t)

;; which-key for command discovery
(use-package which-key :ensure t
  :init
  (which-key-mode)
)

;; Code folding based on newer treesit.el
(use-package treesit-fold :ensure t
  :bind
  (:map treesit-fold-mode-map
    ("C-c C-f" . treesit-fold-toggle)
    ("C-c C-u" . treesit-fold-unfold-all)
    ("C-c C-r" . treesit-fold-rebuild)
  )
  :custom
  (treesit-fold-line-count-show t)
  :init
  (global-treesit-fold-mode)
  (global-treesit-fold-indicators-mode)
)

(use-package grip-mode
  :ensure t
  :config (setq grip-command 'go-grip) ;; auto, grip, go-grip or mdopen
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
(when (display-graphic-p) (use-package all-the-icons :ensure t))
;; Run all-the-icons-install-fonts afterwards

;; Emojis
(use-package emojify :ensure t
  :custom
  (emojify-emoji-styles '(unicode github))
  (emojify-display-style 'unicode)
  :init
  (global-emojify-mode)
)

;; Theme
(use-package dracula-theme :ensure t) ;; Needed, as catppuccin-theme depends on it
(use-package catppuccin-theme :ensure t
  :custom
  (catppuccin-flavor 'frappe)
  :init
  (load-theme 'catppuccin :no-confirm)
)

;; Keybindings go here
(load-file (expand-file-name "keybindings.el" user-emacs-directory))

;; Manually process elpaca queues now before loading customizations
;; Keybindings may go above this line if they depend on extra packages
(elpaca-process-queues)

;; Disable menu bar
(menu-bar-mode -1)

;; Customization
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
