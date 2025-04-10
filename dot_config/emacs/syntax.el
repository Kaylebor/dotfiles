;; -*- lexical-binding: t; -*-
(use-package dbml-mode :ensure t)

(use-package tree-sitter :ensure t
  :custom
  ;; Add treesitter grammars so Emacs knows where to find them
  (treesit-language-source-alist'((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
                                  (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.5.0")
                                  (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
                                  (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "v0.3.4")
                                  (elisp "https://github.com/Wilfred/tree-sitter-elisp" "main")
                                  (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
                                  (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
                                  (gosum "https://github.com/tree-sitter-grammars/tree-sitter-go-sum" "v1.0.0")
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
  :init
  ;; Map file extensions to tree-sitter modes
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(ex\\|exs\\|eex\\|leex\\|heex\\)\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
  (add-to-list 'auto-mode-alist '("\\(\\.go\\|/go\\.mod\\|/go\\.sum\\)\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.dbml\\'" . dbml-mode))
  (add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(sh\\|bash\\(rc\\)?\\|zsh\\(rc\\)?\\|rc\\)\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.fish\\(rc\\)?\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\(?:\\(?:c\\|ld\\|5\\|net\\)\\)?\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(ya?ml\\)\\(?:-cpp\\)?\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.hcl[2-4]?\\'" . hcl-ts-mode)))

;; eglot configurations
(use-package eglot :ensure t
  :hook
  (ruby-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (json-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  :custom
  (eglot-server-programs `(ruby-ts-mode . ("solargraph" "stdio"))
                         `(typescript-ts-mode . ("typescript-language-server" "--stdio" :initializationOptions '(:importModuleSpecifierPreference "project-relative")))
                         `(json-ts-mode . ("vscode-json-language-server"))
                         `(go-ts-mode . ("gopls"))))

;; breadcrumbs
(use-package breadcrumb :ensure t
  :bind
  (("C-c o" . breadcrumb-jump))
  :init
  (breadcrumb-mode 1))
