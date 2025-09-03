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
(global-set-key (kbd "M-g f") 'consult-flycheck)

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
(global-set-key (kbd "C-c t") 'vterm)

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

;; Claude Code IDE vterm integration
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-c") #'claude-code-ide-send-escape))

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

;;; Which-key descriptions

(when (fboundp 'which-key-add-key-based-replacements)
  ;; Main prefixes
  (which-key-add-key-based-replacements "C-c" "user")
  (which-key-add-key-based-replacements "C-c f" "files")
  (which-key-add-key-based-replacements "C-c l" "AI/LLM")
  (which-key-add-key-based-replacements "C-c o" "org")
  (which-key-add-key-based-replacements "C-c s" "search")
  (which-key-add-key-based-replacements "C-c t" "terminal")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x t" "treemacs")
  (which-key-add-key-based-replacements "M-s" "search")
  (which-key-add-key-based-replacements "M-g" "goto")
  
  ;; Nested descriptions for file operations
  (which-key-add-key-based-replacements "C-c f f" "find-file")
  (which-key-add-key-based-replacements "C-c f s" "save-buffer")
  (which-key-add-key-based-replacements "C-c f S" "save-some")
  (which-key-add-key-based-replacements "C-c f r" "recent")
  (which-key-add-key-based-replacements "C-c f d" "dired")
  (which-key-add-key-based-replacements "C-c f D" "dired-other")
  (which-key-add-key-based-replacements "C-c f R" "rename")
  (which-key-add-key-based-replacements "C-c f c" "copy")
  (which-key-add-key-based-replacements "C-c f w" "write-as")
  
  ;; Nested descriptions for AI/LLM
  (which-key-add-key-based-replacements "C-c l l" "gptel-menu")
  (which-key-add-key-based-replacements "C-c l s" "send")
  (which-key-add-key-based-replacements "C-c l a" "add")
  (which-key-add-key-based-replacements "C-c l f" "add-file")
  (which-key-add-key-based-replacements "C-c l d" "debug")
  (which-key-add-key-based-replacements "C-c l p" "preset")
  (which-key-add-key-based-replacements "C-c l g" "aider")
  (which-key-add-key-based-replacements "C-c l c" "claude-menu")
  (which-key-add-key-based-replacements "C-c l C" "claude-start")
  (which-key-add-key-based-replacements "C-c l r" "claude-resume")
  (which-key-add-key-based-replacements "C-c l b" "claude-buffer")
  (which-key-add-key-based-replacements "C-c l q" "claude-stop")
  (which-key-add-key-based-replacements "C-c l w" "claude-toggle")
  (which-key-add-key-based-replacements "C-c l i" "claude-insert")
  (which-key-add-key-based-replacements "C-c l 1" "switch-coder")
  (which-key-add-key-based-replacements "C-c l 2" "switch-agentic")
  
  ;; Nested descriptions for Org
  (which-key-add-key-based-replacements "C-c o f" "find-org")
  (which-key-add-key-based-replacements "C-c o a" "agenda")
  (which-key-add-key-based-replacements "C-c o c" "capture")
  (which-key-add-key-based-replacements "C-c o l" "store-link")
  (which-key-add-key-based-replacements "C-c o L" "insert-link")
  (which-key-add-key-based-replacements "C-c o t" "todo")
  (which-key-add-key-based-replacements "C-c o s" "schedule")
  (which-key-add-key-based-replacements "C-c o d" "deadline")
  (which-key-add-key-based-replacements "C-c o o" "open-index"))

;; Create aliases for Meow's keypad display
;; These must be defined after the keymaps are created
(defalias 'meow-ai-llm-map ai-llm-map)
(defalias 'meow-file-map file-map)
(defalias 'meow-org-map org-map)

(provide 'keybindings)