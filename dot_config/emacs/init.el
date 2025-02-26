;; -*- lexical-binding: t; -*-
;; Setup elpaca package manager https://github.com/progfolio/elpaca/blob/master/doc/manual.md#usage
(load-file (expand-file-name "elpaca-init.el" user-emacs-directory))

;; Personal functions; some may be used in configurations
(load-file (expand-file-name "functions.el" user-emacs-directory))

;; PACKAGES
;; Improves some MacOS defaults
(when (or (daemonp) (memq window-system '(mac ns)))
  (elpaca exec-path-from-shell (exec-path-from-shell-initialize)))

;; corfu for improved completion UI
(elpaca corfu
  (global-corfu-mode))

;; vertico for improved completion UI
(elpaca vertico
  (vertico-mode)
  (vertico-mouse-mode))

;; consult provides a bunch of search commands to vertico
(elpaca consult)

;; embark provides contextual actions to vertico
(elpaca embark
  (setopt prefix-help-command #'embark-prefix-help-command))
(elpaca embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;; orderless for improved completion matching
(elpaca orderless
  (setopt completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia to add completion information
(elpaca marginalia
  (marginalia-mode))

;; ibuffer-vc
(elpaca ibuffer-vc)

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
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; magit
(elpaca magit)

;; transient
(elpaca transient)

;; project.el
(elpaca project)

;; which-key for command discovery
(elpaca which-key
  (which-key-mode))

;; Code folding based on newer treesit.el
(elpaca treesit-fold)

;; Better mise integration with Emacs
(elpaca mise (global-mise-mode))

;; flycheck
(elpaca flycheck
  (global-flycheck-mode))

;; eglot configurations
(elpaca eglot
  (add-hook 'ruby-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'json-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook 'eglot-ensure))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    `(ruby-ts-mode . ("solargraph" "stdio"))
    `(typescript-ts-mode . ("typescript-language-server" "--stdio" :initializationOptions '(:importModuleSpecifierPreference "project-relative")))
    `(json-ts-mode . ("vscode-json-language-server"))
    `(go-ts-mode . ("gopls"))))

;; DAP debugging
(elpaca dape)

;; 1Password integration
(elpaca (auth-source-1password :host github :repo "dlobraico/auth-source-1password" :build t)
  (auth-source-1password-enable))

;; GPT and family
(elpaca gptel
  (setopt gptel-model 'google/gemini-2.0-pro-exp-02-05:free
          gptel-backend
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
                        google/gemini-2.0-flash-lite-preview-02-05:free))))

;; icons
(when (display-graphic-p)
  (elpaca all-the-icons))
;; Run all-the-icons-install-fonts afterwards

;; Emojis
(elpaca emojify
  (global-emojify-mode))

;; Enforce dbml-mode on .dbml files
(elpaca dbml-mode
  (add-to-list 'auto-mode-alist '("\\.dbml\\'" . dbml-mode)))

;; Theme
(elpaca dracula-theme
  (load-theme 'dracula t))

;; Keybindings go here
(load-file (expand-file-name "keybindings.el" user-emacs-directory))

;; Manually process elpaca queues now before loading customizations
;; Keybindings may go above this line if they depend on extra packages
(elpaca-process-queues)

;; Add treesitter grammars so Emacs knows where to find them
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
     (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.5.0")
     (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp" "main")
     (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
     (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
     (make "https://github.com/tree-sitter-grammars/tree-sitter-make" "v1.1.1")
     (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.3.2" "tree-sitter-markdown/src")
     (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.3.2" "tree-sitter-markdown-inline/src")
     (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
     (toml "https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
     (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0")))

;; Forcefully enable some built-in major modes on certain file extensions
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

;; Customization
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
