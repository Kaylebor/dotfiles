;; -*- lexical-binding: t; -*-
;; Centralized keybindings configuration
;; This file is loaded after all packages via elpaca-after-init-hook

;;; Global Navigation and Editing

;; Window navigation with Shift+arrow
(windmove-default-keybindings)

;; Consult - Enhanced search and navigation
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s f") 'consult-find)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "M-g f") 'consult-flymake)

;; Avy - Jump navigation
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g l") 'avy-goto-line)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; Embark - Contextual actions
(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "C-;") 'embark-dwim)
(global-set-key (kbd "C-h B") 'embark-bindings)

;;; Help System

;; Helpful - Better help buffers
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h x") 'helpful-command)

;;; Development Tools

;; Treemacs - File explorer
(global-set-key (kbd "C-x t") 'treemacs)

;; Terminal
(global-set-key (kbd "C-c t") 'eat)

;; Ripgrep search
(global-set-key (kbd "C-c s") 'rg-menu)

;;; Mode-specific Keybindings

;; Completion preview
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;; Corfu completion
(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "TAB") 'corfu-next)
  (define-key corfu-map [tab] 'corfu-next)
  (define-key corfu-map (kbd "S-TAB") 'corfu-previous)
  (define-key corfu-map [backtab] 'corfu-previous)
  (define-key corfu-map (kbd "RET") 'corfu-insert)
  (define-key corfu-map (kbd "M-SPC") 'corfu-insert-separator))

;; Magit difftastic integration
(with-eval-after-load 'magit-blame
  (define-key magit-blame-read-only-mode-map (kbd "D") 'difftastic-magit-show)
  (define-key magit-blame-read-only-mode-map (kbd "S") 'difftastic-magit-show))

;; Treesit folding
(with-eval-after-load 'treesit-fold
  (define-key treesit-fold-mode-map (kbd "C-c C-f") 'treesit-fold-toggle)
  (define-key treesit-fold-mode-map (kbd "C-c C-u") 'treesit-fold-unfold-all)
  (define-key treesit-fold-mode-map (kbd "C-c C-r") 'treesit-fold-rebuild))

;; Claude Code IDE vterm integration - DISABLED (using eat now)
;; (with-eval-after-load 'vterm
;;   (define-key vterm-mode-map (kbd "C-c C-c") #'claude-code-ide-send-escape))

;;; Prefix Keymaps for Organization

;; File operations keymap (C-c f)
(defvar file-map (make-sparse-keymap)
  "Keymap for file operations")
(define-key file-map "f" 'find-file)
(define-key file-map "s" 'save-buffer)
(define-key file-map "S" 'save-some-buffers)
(define-key file-map "r" 'consult-recent-file)
(define-key file-map "d" 'dired)
(define-key file-map "D" 'dired-other-window)
(define-key file-map "R" 'rename-file)
(define-key file-map "c" 'copy-file)
(define-key file-map "w" 'write-file)
(global-set-key (kbd "C-c f") file-map)

;; AI/LLM tools keymap (C-c l)
(defvar ai-llm-map (make-sparse-keymap)
  "Keymap for AI/LLM tools")
(define-key ai-llm-map "l" 'gptel-menu)
(define-key ai-llm-map "s" 'gptel-send)
(define-key ai-llm-map "a" 'gptel-add)
(define-key ai-llm-map "f" 'gptel-add-file)
(define-key ai-llm-map "d" 'gptel-multimodal-debug)
(define-key ai-llm-map "p" 'gptel-quick-preset)
(define-key ai-llm-map "g" 'aidermacs-transient-menu)
(define-key ai-llm-map "c" 'claude-code-ide-menu)
(define-key ai-llm-map "C" 'claude-code-ide)
(define-key ai-llm-map "r" 'claude-code-ide-resume)
(define-key ai-llm-map "b" 'claude-code-ide-switch-to-buffer)
(define-key ai-llm-map "q" 'claude-code-ide-stop)
(define-key ai-llm-map "w" 'claude-code-ide-toggle-window)
(define-key ai-llm-map "i" 'claude-code-ide-insert-at-mentioned)
;; Conditional model switching (when Ollama is available)
(when (fboundp 'gptel-switch-to-coder)
  (define-key ai-llm-map "1" 'gptel-switch-to-coder))
(when (fboundp 'gptel-switch-to-agentic)
  (define-key ai-llm-map "2" 'gptel-switch-to-agentic))
(global-set-key (kbd "C-c l") ai-llm-map)

;; Org mode keymap (C-c o)
(defvar org-map (make-sparse-keymap)
  "Keymap for Org mode commands")
(define-key org-map "f" 'my/consult-org-files)
(define-key org-map "a" 'org-agenda)
(define-key org-map "c" 'org-capture)
(define-key org-map "l" 'org-store-link)
(define-key org-map "L" 'org-insert-link)
(define-key org-map "t" 'org-todo)
(define-key org-map "s" 'org-schedule)
(define-key org-map "d" 'org-deadline)
(define-key org-map "o" (lambda () (interactive) 
                          (find-file "~/org/index.org")))
(global-set-key (kbd "C-c o") org-map)

;;; Which-key descriptions are now in which-key-replacements.el

;; Create aliases for Meow's keypad display (only if Meow is loaded)
;; These must be defined after the keymaps are created
(when (featurep 'meow)
  (defalias 'meow-ai-llm-map ai-llm-map)
  (defalias 'meow-file-map file-map)
  (defalias 'meow-org-map org-map))

(provide 'keybindings)