;; -*- lexical-binding: t; -*-
;; Setup elpaca package manager https://github.com/progfolio/elpaca/blob/master/doc/manual.md#usage
(load-file (expand-file-name "elpaca-init.el" user-emacs-directory))

;; Personal functions; some may be used in configurations
(load-file (expand-file-name "functions.el" user-emacs-directory))

;; PACKAGES
;; Improves some MacOS defaults
(when (or (daemonp) (memq window-system '(mac ns)))
  (elpaca exec-path-from-shell (exec-path-from-shell-initialize)))

;; corfu for improved completion UI
(elpaca corfu
  (global-corfu-mode))

;; vertico for improved completion UI
(elpaca vertico
  (vertico-mode)
  (vertico-mouse-mode))

;; consult provides a bunch of search commands to vertico
(elpaca consult)

;; embark provides contextual actions to vertico
(elpaca embark
  (setopt prefix-help-command #'embark-prefix-help-command))
(elpaca embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;; orderless for improved completion matching
(elpaca orderless
  (setopt completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia to add completion information
(elpaca marginalia
  (marginalia-mode))

;; ibuffer-vc
(elpaca ibuffer-vc)

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; magit
(elpaca magit)

;; project.el
(elpaca project)

;; which-key for command discovery
(elpaca which-key
  (which-key-mode))

;; Code folding based on newer treesit.el
(elpaca treesit-fold)

;; Better mise integration with Emacs
(elpaca mise (global-mise-mode))

;; flycheck
(elpaca flycheck
  (global-flycheck-mode))

;; lsp-mode
(elpaca lsp-mode (lsp-modeline-code-actions-mode))

;; 1Password integration
(elpaca (auth-source-1password :host github :repo "dlobraico/auth-source-1password" :build t)
  (auth-source-1password-enable))

;; GPT and family
(elpaca gptel)

;; icons
(when (display-graphic-p)
  (elpaca all-the-icons))
;; Run all-the-icons-install-fonts afterwards

;; Enforce dbml-mode on .dbml files
(elpaca dbml-mode
  (add-to-list 'auto-mode-alist '("\\.dbml\\'" . dbml-mode)))

;; Theme
(elpaca dracula-theme
  (load-theme 'dracula t))

;; Keybindings go here
(load-file (expand-file-name "keybindings.el" user-emacs-directory))

;; Manually process elpaca queues now before loading customizations
;; Keybindings may go above this line if they depend on extra packages
(elpaca-process-queues)

;; Customization
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
