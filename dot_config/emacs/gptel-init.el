;; -*- lexical-binding: t; -*-
(use-package gptel :ensure t
  :bind
  (("C-c l l" . gptel-menu)
   ("C-c l s" . gptel-send)
   ("C-c l a" . gptel-add)
   ("C-c l f" . gptel-add-file))
  :custom
  (gptel-model 'google/gemini-2.0-flash-exp)
  (gptel-backend
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pick-first-password :host "OpenRouter" :user "API Token"))
      :models '(deepseek/deepseek-r1:free
                deepseek/deepseek-r1
                google/gemini-2.0-flash-exp
                google/gemini-2.0-pro-exp-02-05:free
                google/gemini-2.0-flash-thinking-exp:free
                google/gemini-2.0-flash-lite-preview-02-05:free
                google/gemini-2.5-pro-exp-03-25:free))))

(use-package aidermacs :ensure t
  :bind (("C-c g" . aidermacs-transient-menu))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "google/gemini-2.0-flash-exp")
  :hook
  (aidermacs-before-run-backend . (lambda () (setenv "OPENROUTER_API_KEY" (auth-source-pick-first-password :host "OpenRouter" :user "API Token")))))

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el")
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion))
  :config
  (add-to-list 'copilot-major-mode-alist '("ruby-ts-mode" . "ruby"))
  (add-to-list 'copilot-major-mode-alist '("python-ts-mode" . "python"))
  (add-to-list 'copilot-major-mode-alist '("javascript-ts-mode" . "javascript"))
  (add-to-list 'copilot-major-mode-alist '("js-jsx-mode" . "javascriptreact"))
  (add-to-list 'copilot-major-mode-alist '("typescript-ts-mode" . "typescript"))
  (add-to-list 'copilot-major-mode-alist '("tsx-ts-mode" . "typescriptreact"))
  (add-to-list 'copilot-major-mode-alist '("json-ts-mode" . "json"))
  (add-to-list 'copilot-major-mode-alist '("html-ts-mode" . "html"))
  (add-to-list 'copilot-major-mode-alist '("css-ts-mode" . "css"))
  (add-to-list 'copilot-major-mode-alist '("yaml-ts-mode" . "yaml"))
  (add-to-list 'copilot-major-mode-alist '("markdown-ts-mode" . "markdown"))
  (add-to-list 'copilot-major-mode-alist '("bash-ts-mode" . "shellscript"))
  (add-to-list 'copilot-major-mode-alist '("sh-mode" . "shellscript"))
  (add-to-list 'copilot-major-mode-alist '("go-ts-mode" . "go"))
  :hook
  (prog-mode . copilot-mode))
