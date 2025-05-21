;; -*- lexical-binding: t; -*-
(use-package gptel :ensure t
  :bind
  (("C-c C-l l" . gptel-menu)
   ("C-c C-l s" . gptel-send)
   ("C-c C-l a" . gptel-add)
   ("C-c C-l f" . gptel-add-file))
  :custom
  (gptel-model 'deepseek/deepseek-chat-v3-0324:free)
  (gptel-backend
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pick-first-password :host "OpenRouter" :user "API Token"))
      :models '(deepseek/deepseek-r1:free
                deepseek/deepseek-r1
                microsoft/mai-ds-r1:free
                perplexity/r1-1776
                deepseek/deepseek-v3-base:free
                deepseek/deepseek-chat-v3-0324:free
                google/gemini-2.0-flash-exp:free
                google/gemini-2.0-flash-thinking-exp:free
                google/gemini-2.0-flash-001
                google/gemini-2.0-flash-lite-001
                google/gemini-2.5-flash-preview
                google/gemini-2.5-flash-preview:thinking
                google/gemini-2.5-pro-exp-03-25
                google/gemini-2.5-pro-preview-03-25))))

(use-package aidermacs :ensure t
  :bind (("C-c g" . aidermacs-transient-menu))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "deepseek/deepseek-chat-v3-0324:free")
  :hook
  (aidermacs-before-run-backend . (lambda () (setenv "OPENROUTER_API_KEY" (auth-source-pick-first-password :host "OpenRouter" :user "API Token")))))
