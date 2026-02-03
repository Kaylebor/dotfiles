;; -*- lexical-binding: t; -*-

(defun gptel-api-key-env-or-auth-source (&optional var host user)
  (lambda ()
    (or (and var (getenv var))
        (auth-source-pick-first-password :host host
                                         :user (or user user-login-name)))))

(use-package gptel :ensure t
  :custom
  ;; Default backend/model: Synthetic Kimi K2 Thinking (excellent reasoning).
  (gptel-backend
   (gptel-make-openai "Synthetic"
     :host "api.synthetic.new"
     :endpoint "/openai/v1/chat/completions"
     :stream t
     :key (gptel-api-key-env-or-auth-source "SYNTHETIC_API_KEY" "Synthetic" "API Token")
     :models '((hf:moonshotai/Kimi-K2.5
                :description "Kimi K2.5 - Excellent reasoning and long context, SOTA"
                :capabilities (media tool-use json reasoning)
                :mime-types ("image/jpeg"
                             "image/png"
                             "image/gif"
                             "image/webp"
                             "image/heif"
                             "image/heic"))
               (hf:zai-org/GLM-4.7
                :description "GLM-4.7 - Strong tool use and structured outputs"
                :capabilities (tool-use json reasoning))))
   (gptel-model 'hf:moonshotai/Kimi-K2.5)
   (gptel-use-tools t)
   (gptel-confirm-tool-calls 'always)))

(elpaca (gptel-agent :host github :repo "karthink/gptel-agent" :branch "master"
                     :files (:defaults "agents"))
  ;; Safely configure agents directory after package loads
  (with-eval-after-load 'gptel-agent
    (add-to-list 'gptel-agent-dirs
                 (expand-file-name "~/.config/emacs/gptel-agents"))
    (gptel-agent-update)))
