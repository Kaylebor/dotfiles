;;; which-key-replacements.el --- Which-key descriptions for all keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidated which-key replacements loaded after all packages and keybindings
;; This file is loaded via elpaca-after-init-hook with priority 99, ensuring
;; all keymaps and bindings exist before we describe them.

;;; Code:

;; Safety check - only proceed if which-key is available
(when (fboundp 'which-key-add-key-based-replacements)

  ;;; Built-in Emacs prefixes

  ;; Core Emacs prefixes
  (which-key-add-key-based-replacements "C-h" "help")
  (which-key-add-key-based-replacements "C-c" "user")
  (which-key-add-key-based-replacements "M-g" "goto")
  (which-key-add-key-based-replacements "M-s" "search")
  (which-key-add-key-based-replacements "ESC ESC" "escape-commands")

  ;; C-x prefixes
  (which-key-add-key-based-replacements "C-x 4" "other-window")
  (which-key-add-key-based-replacements "C-x 5" "other-frame")
  (which-key-add-key-based-replacements "C-x 6" "two-column")
  (which-key-add-key-based-replacements "C-x 8" "unicode/emoji")
  (which-key-add-key-based-replacements "C-x 8 e" "emoji")
  (which-key-add-key-based-replacements "C-x a" "abbreviations")
  (which-key-add-key-based-replacements "C-x n" "narrow/widen")
  (which-key-add-key-based-replacements "C-x r" "register/bookmark")
  (which-key-add-key-based-replacements "C-x v" "version-control")
  (which-key-add-key-based-replacements "C-x w" "window-layout")
  (which-key-add-key-based-replacements "C-x x" "buffer-tools")
  (which-key-add-key-based-replacements "C-x C-a" "debugger")
  (which-key-add-key-based-replacements "C-x C-k" "keyboard-macros")
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x ESC" "repeat-command")

  ;; Nested version control prefixes
  (which-key-add-key-based-replacements "C-x v M" "vc-merge")
  (which-key-add-key-based-replacements "C-x v b" "vc-branch")
  (which-key-add-key-based-replacements "C-x v w" "vc-worktree")

  ;; Nested window layout prefixes
  (which-key-add-key-based-replacements "C-x w ^" "window-detach")
  (which-key-add-key-based-replacements "C-x w f" "window-flip")
  (which-key-add-key-based-replacements "C-x w o" "window-rotate")
  (which-key-add-key-based-replacements "C-x w r" "window-rotate-clockwise")

  ;; Nested keyboard macro prefixes
  (which-key-add-key-based-replacements "C-x C-k C-q" "kmacro-quit-counter")
  (which-key-add-key-based-replacements "C-x C-k C-r" "kmacro-register-counter")
  (which-key-add-key-based-replacements "C-x C-k C-r a" "kmacro-register-add")


  ;;; Package-specific prefixes

  ;; Project.el / Projectile
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x p 4" "project-other-window")
  (which-key-add-key-based-replacements "C-x p 5" "project-other-frame")
  (which-key-add-key-based-replacements "C-x p c" "project-compile")
  (which-key-add-key-based-replacements "C-x p s" "project-search")
  (which-key-add-key-based-replacements "C-x p x" "project-shell")
  (which-key-add-key-based-replacements "C-x p C-x" "project-save")
  (which-key-add-key-based-replacements "C-x p x 4" "project-shell-other-window")

  ;; Magit & Forge
  (which-key-add-key-based-replacements "C-x g" "magit")
  (which-key-add-key-based-replacements "C-x g '" "forge")
  (which-key-add-key-based-replacements "C-x g N" "forge-create")

  ;; Treemacs
  (which-key-add-key-based-replacements "C-x t" "treemacs")

  ;; Flycheck
  (which-key-add-key-based-replacements "C-c !" "flycheck")

  ;; Yasnippet
  (which-key-add-key-based-replacements "C-c &" "yasnippet")

  ;; Mode-specific
  (which-key-add-key-based-replacements "C-c ^" "mode-specific")

  ;; Evil mode (if applicable)
  (which-key-add-key-based-replacements "g l" "AI-tools")


  ;;; User-defined C-c prefixes

  ;; File operations
  (which-key-add-key-based-replacements "C-c f" "files")
  (which-key-add-key-based-replacements "C-c f f" "find-file")
  (which-key-add-key-based-replacements "C-c f s" "save-buffer")
  (which-key-add-key-based-replacements "C-c f S" "save-some")
  (which-key-add-key-based-replacements "C-c f r" "recent")
  (which-key-add-key-based-replacements "C-c f d" "dired")
  (which-key-add-key-based-replacements "C-c f D" "dired-other")
  (which-key-add-key-based-replacements "C-c f R" "rename")
  (which-key-add-key-based-replacements "C-c f c" "copy")
  (which-key-add-key-based-replacements "C-c f w" "write-as")

  ;; AI/LLM operations
  (which-key-add-key-based-replacements "C-c l" "AI/LLM")
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

  ;; GPTel specific
  (which-key-add-key-based-replacements "C-c C-g" "gptel-switch")
  (which-key-add-key-based-replacements "C-c C-l" "gptel")

  ;; Org mode
  (which-key-add-key-based-replacements "C-c o" "org")
  (which-key-add-key-based-replacements "C-c o f" "find-org")
  (which-key-add-key-based-replacements "C-c o a" "agenda")
  (which-key-add-key-based-replacements "C-c o c" "capture")
  (which-key-add-key-based-replacements "C-c o l" "store-link")
  (which-key-add-key-based-replacements "C-c o L" "insert-link")
  (which-key-add-key-based-replacements "C-c o t" "todo")
  (which-key-add-key-based-replacements "C-c o s" "schedule")
  (which-key-add-key-based-replacements "C-c o d" "deadline")
  (which-key-add-key-based-replacements "C-c o o" "open-index")

  ;; Search
  (which-key-add-key-based-replacements "C-c s" "search")

  ;; Terminal
  (which-key-add-key-based-replacements "C-c t" "terminal")

  (message "Which-key replacements loaded successfully"))

(provide 'which-key-replacements)
;;; which-key-replacements.el ends here