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

(use-package emacs-lisp-mode :mode ("\\.el\\'" . emacs-lisp-mode))

(use-package bash-ts-mode :mode ("\\.\\(sh\\|bash\\(rc\\)?\\|zsh\\(rc\\)?\\|rc\\)\\'" . bash-ts-mode))
(use-package sh-mode :mode ("\\.fish\\(rc\\)?\\'" . sh-mode))

(use-package json-ts-mode :mode ("\\.json\\(?:\\(?:c\\|ld\\|5\\|net\\)\\)?\\'" . json-ts-mode))
(use-package yaml-ts-mode :mode ("\\.\\(ya?ml\\)\\(?:-cpp\\)?\\'" . yaml-ts-mode))
(use-package toml-ts-mode :mode ("\\.toml\\'" . toml-ts-mode))

(use-package sql-ts-mode :mode ("\\.sql\\'" . sql-ts-mode))
(use-package dbml-mode :mode ("\\.dbml\\'" . dbml-mode))

(use-package hcl-ts-mode :mode ("\\.hcl[2-4]?\\'" . hcl-ts-mode))

(use-package markdown-ts-mode :mode ("\\.md\\'" . markdown-ts-mode))

(use-package html-ts-mode :mode ("\\.html\\'" . html-ts-mode))
(use-package css-ts-mode :mode ("\\.css\\'" . css-ts-mode))
(use-package javascript-ts-mode :mode (("\\.js\\'" . javascript-ts-mode)
                                       ("\\.jsx\\'" . js-jsx-mode)))

(use-package typescript-ts-mode :mode (("\\.ts\\'" . typescript-ts-mode)
                                       ("\\.tsx\\'" . tsx-ts-mode)))

(use-package elixir-ts-mode :mode ("\\.\\(ex\\|exs\\|eex\\|leex\\|heex\\)\\'" . elixir-ts-mode))
(use-package python-ts-mode :mode ("\\.py\\'" . python-ts-mode))
(use-package go-ts-mode :mode ("\\(\\.go\\|/go\\.mod\\|/go\\.sum\\)\\'" . go-ts-mode))

(use-package gdscript-mode :ensure (:host github :repo "godotengine/emacs-gdscript-mode") :mode ("\\.gd\\'" . gdscript-ts-mode))

;; Linting
(use-package flycheck :ensure t
  :init
  (global-flycheck-mode))

;; LSP support
(use-package lsp-mode :ensure t
  :custom
  (setopt lsp-keymap-prefix "C-c l")
  (lsp-copilot-enabled t)
  (lsp-keep-workspace-alive nil) ;; close LSP servers after last buffer is closed
  :hook
  (ruby-ts-mode . lsp)
  (html-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (json-ts-mode . lsp)
  (go-ts-mode . lsp)
  (elixir-ts-mode . lsp)
  (gdscript-ts-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration))
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
