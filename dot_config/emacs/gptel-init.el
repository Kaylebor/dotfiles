;; -*- lexical-binding: t; -*-
(use-package gptel :ensure t
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
