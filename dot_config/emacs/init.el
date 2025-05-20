;; -*- lexical-binding: t; -*-

;; Suppress warnings
(setopt warning-minimum-level :error)

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

;; TreeSitter and lsp
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

;; icons
(use-package all-the-icons :ensure t)
;; Run all-the-icons-install-fonts afterwards

;; treemacs for visual navigation
(use-package treemacs :ensure t)
(use-package treemacs-evil :ensure t)
(use-package treemacs-projectile :ensure t)
(use-package treemacs-magit :ensure t)
(use-package treemacs-nerd-icons :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

;; helm for incremental completion
(use-package helm :ensure t
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("M-x" . helm-M-x))
  :custom
  (completions-detailed t)
  (helm-mode-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  :config
  (helm-mode 1)
  (helm-popup-tip-mode)
  (helm-ff-icon-mode))
(use-package helm-projectile :ensure t
  :bind
  (("C-x p p" . helm-projectile-switch-project)
   ("C-x p f" . helm-projectile-find-file)
   ("C-x p b" . helm-projectile-list-buffers)))

;; company for completion UI
(use-package company :ensure t
  :config
  (global-company-mode))

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

;; Spelling
(use-package jinx :ensure t)

;; Better mise integration with Emacs
(use-package mise :ensure t
  :init
  (global-mise-mode))

;; 1Password integration
(elpaca (auth-source-1password :host github :repo "dlobraico/auth-source-1password" :build t)
  (auth-source-1password-enable))

;; GPT and family
(load-file (expand-file-name "gptel-init.el" user-emacs-directory))

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

;; Customization
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
