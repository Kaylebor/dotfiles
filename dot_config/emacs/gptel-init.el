;; -*- lexical-binding: t; -*-
(use-package gptel :ensure t
  :bind
  (
   ("C-c l l" . gptel-menu)
   ("C-c l s" . gptel-send)
   ("C-c l a" . gptel-add)
   ("C-c l f" . gptel-add-file)
  )
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
                google/gemini-2.5-pro-exp-03-25:free)
    )
  )
)

(use-package aidermacs
  :bind (("C-c g" . aidermacs-transient-menu))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "google/gemini-2.0-flash-exp")
  :hook
  (aidermacs-before-run-backend . (lambda () (setenv "OPENROUTER_API_KEY" (auth-source-pick-first-password :host "OpenRouter" :user "API Token")))))
