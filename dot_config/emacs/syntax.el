;; -*- lexical-binding: t; -*-
(use-package dbml-mode :ensure t)

(use-package tree-sitter :ensure t
  :init
  ;; Add treesitter grammars so Emacs knows where to find them
  (setopt treesit-language-source-alist'((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
                                         (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.5.0")
                                         (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
                                         (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "v0.3.4")
                                         (elisp "https://github.com/Wilfred/tree-sitter-elisp" "main")
                                         (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
                                         (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
                                         (gosum "https://github.com/tree-sitter-grammars/tree-sitter-go-sum" "v1.0.0")
                                         (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript" "v2.0.0")
                                         (ruby "https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1")
                                         (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
                                         (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
                                         (jsx "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src")
                                         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src")
                                         (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
                                         (make "https://github.com/alemuller/tree-sitter-make" "main")
                                         (markdown "https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1")
                                         (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
                                         (toml "https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1")
                                         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
                                         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
                                         (yaml "https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")
                                         (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl" "v1.1.0")))
  ;; Map file extensions to tree-sitter modes
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

(use-package emacs-lisp-mode :delight :mode ("\\.el\\'" . emacs-lisp-mode))

(use-package bash-ts-mode :delight :mode ("\\.\\(sh\\|bash\\(rc\\)?\\|zsh\\(rc\\)?\\|rc\\)\\'" . bash-ts-mode))
(use-package sh-mode :delight :mode ("\\.fish\\(rc\\)?\\'" . sh-mode))

(use-package json-ts-mode :delight :mode ("\\.json\\(?:\\(?:c\\|ld\\|5\\|net\\)\\)?\\'" . json-ts-mode))
(use-package yaml-ts-mode :delight :mode ("\\.\\(ya?ml\\)\\(?:-cpp\\)?\\'" . yaml-ts-mode))
(use-package toml-ts-mode :delight :mode ("\\.toml\\'" . toml-ts-mode))

(use-package sql-ts-mode :delight :mode ("\\.sql\\'" . sql-ts-mode))
(use-package dbml-mode :delight :mode ("\\.dbml\\'" . dbml-mode))

(use-package hcl-ts-mode :delight :mode ("\\.hcl[2-4]?\\'" . hcl-ts-mode))

(use-package markdown-ts-mode :delight :mode ("\\.md\\'" . markdown-ts-mode))

(use-package html-ts-mode :delight :mode ("\\.html\\'" . html-ts-mode))
(use-package css-ts-mode :delight :mode ("\\.css\\'" . css-ts-mode))
(use-package javascript-ts-mode :delight :mode (("\\.js\\'" . javascript-ts-mode)
                                       ("\\.jsx\\'" . js-jsx-mode)))

(use-package typescript-ts-mode :delight :mode (("\\.ts\\'" . typescript-ts-mode)
                                       ("\\.tsx\\'" . tsx-ts-mode)))

(use-package elixir-ts-mode :delight :mode ("\\.\\(ex\\|exs\\|eex\\|leex\\|heex\\)\\'" . elixir-ts-mode))
(use-package python-ts-mode :delight :mode ("\\.py\\'" . python-ts-mode))
(use-package go-ts-mode :delight :mode ("\\(\\.go\\|/go\\.mod\\|/go\\.sum\\)\\'" . go-ts-mode))

(use-package gdscript-mode :ensure (:host github :repo "godotengine/emacs-gdscript-mode") :delight :mode ("\\.gd\\'" . gdscript-ts-mode))

;; Linting
(use-package flycheck :ensure t
  :delight
  :init
  (global-flycheck-mode)
  :config
  ;; Disable Ruby checkers that overlap with Ruby LSP
  (setq-default flycheck-disabled-checkers '(ruby-rubocop ruby-standard)))

;; LSP support
(use-package lsp-mode :ensure t
  :delight
  :custom
  (setopt lsp-keymap-prefix "C-c l")
  (lsp-keep-workspace-alive nil) ;; close LSP servers after last buffer is closed
  (lsp-ruby-lsp-use-bundler nil) ;; use global ruby-lsp with composed bundle
  :hook
  (ruby-ts-mode . lsp)
  (html-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (go-ts-mode . lsp)
  (elixir-ts-mode . lsp)
  (gdscript-ts-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  ;; Disable TypeProf and default ruby-lsp to avoid conflicts
  (add-to-list 'lsp-disabled-clients 'typeprof-ls)
  (add-to-list 'lsp-disabled-clients 'ruby-lsp-ls)
  ;; Register custom ruby-lsp client with launcher for dependency handling
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection 
                     (lambda () 
                       (let ((default-directory (or (lsp-workspace-root) default-directory)))
                         (list (my-mise-which "ruby-lsp") "--use-launcher"))))
    :activation-fn (lsp-activate-on "ruby")
    :priority 10
    :server-id 'ruby-lsp-mise))
  
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (append '("emacs-lsp-booster" "--disable-bytecode" "--") orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))
(use-package lsp-treemacs :ensure t
  :custom
  (lsp-treemacs-theme "nerd-icons"))
(use-package lsp-ui :ensure t)
(use-package helm-lsp :ensure t)

;; DAP debugging
(use-package dap-mode :ensure t)

;; Better projectile integration for Rails
(use-package projectile-rails :ensure t
  :init
  (projectile-rails-global-mode))
