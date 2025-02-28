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

;; project.el
(use-package project :ensure t)

;; completion-preview for minimal completion suggestions (built-in)
(use-package completion-preview :ensure t
  :init
  (global-completion-preview-mode)
)

;; corfu for improved completion UI
(use-package corfu :ensure t
  :init
  (global-corfu-mode)
)

;; vertico for improved completion UI https://github.com/minad/vertico
(use-package vertico :ensure t
  :config
  (vertico-mode)
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
(use-package gptel :ensure t
  :custom
  (gptel-model 'google/gemini-2.0-pro-exp-02-05:free)
  (gptel-backend
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pick-first-password :host "OpenRouter" :user "API Token"))
      :models '(deepseek/deepseek-r1:free
                deepseek/deepseek-r1
                openai/gpt-3.5-turbo
                mistralai/mixtral-8x7b-instruct
                meta-llama/codellama-34b-instruct
                codellama/codellama-70b-instruct
                google/gemini-2.0-pro-exp-02-05:free
                google/gemini-2.0-flash-thinking-exp:free
                google/gemini-2.0-flash-lite-preview-02-05:free)
    )
  )
)

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
(use-package dracula-theme :ensure t
  :init
  (load-theme 'dracula t)
)

;; Keybindings go here
(load-file (expand-file-name "keybindings.el" user-emacs-directory))

;; Manually process elpaca queues now before loading customizations
;; Keybindings may go above this line if they depend on extra packages
(elpaca-process-queues)

;; Enable tabs
(tab-bar-mode)

;; Disable menu bar
(menu-bar-mode -1)

;; Customization
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
