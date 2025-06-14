;; -*- lexical-binding: t; -*-

;; Allow for shorter responses: "y" for yes and "n" for no.
(setopt read-answer-short t)
(setopt use-short-answers t)
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; Allow nested minibuffers
(setopt enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setopt minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; By default, Emacs "updates" its ui more often than it needs to
(setopt idle-update-delay 1.0)

(setopt ring-bell-function #'ignore)

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setopt truncate-string-ellipsis "…")

;; Delete by moving to trash in interactive mode
(setopt delete-by-moving-to-trash (not noninteractive)
        remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Ignoring this is acceptable since it will redirect to the buffer regardless.
(setopt find-file-suppress-same-file-warnings t)

;; Resolve symlinks so that operations are conducted from the file's directory
(setopt find-file-visit-truename t
        vc-follow-symlinks t)

;; Prefer vertical splits over horizontal ones
(setopt split-width-threshold 170
        split-height-threshold nil)

;; Buffers
(setopt uniquify-buffer-name-style 'forward
        comint-prompt-read-only t
        comint-buffer-maximum-size 2048)

;; Backups
(setopt backup-directory-alist
        `(("." . ,(expand-file-name "backup" user-emacs-directory)))
        tramp-backup-directory-alist backup-directory-alist
        backup-by-copying-when-linked t
        backup-by-copying t
        delete-old-versions t
        version-control t
        kept-new-versions 5
        kept-old-versions 5)

;; VC
(setopt vc-git-print-log-follow t
        vc-make-backup-files nil)

;; Remove duplicates from the kill ring to reduce clutter
(setopt kill-do-not-save-duplicates t)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.
(setopt revert-without-query (list ".")  ; Do not prompt
        auto-revert-stop-on-user-input nil
        auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setopt global-auto-revert-non-file-buffers t
        global-auto-revert-ignore-modes '(Buffer-menu-mode)) 

;; Enables Emacs to remember a list of recently accessed files.
(setopt recentf-max-saved-items 300 ; default is 20
        recentf-max-menu-items 15
        recentf-auto-cleanup (if (daemonp) 300 'never)
        recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))

;; Enables Emacs to remember the last location within a file upon reopening.
(setopt save-place-file (expand-file-name "saveplace" user-emacs-directory)
        save-place-limit 600)

;; Enables Emacs to preserve the minibuffer history between sessions.
(setopt history-length 300
        savehist-save-minibuffer-history t ;; Default
        savehist-additional-variables
        '(kill-ring                        ; clipboard
          register-alist                   ; macros
          mark-ring global-mark-ring       ; marks
          search-ring regexp-search-ring)) ; searches

;;; Scrolling
(setopt fast-but-imprecise-scrolling t
        scroll-error-top-bottom t
        scroll-preserve-screen-position t
        scroll-margin 0
        hscroll-margin 2
        hscroll-step 1)

;;; Mouse
(when (and (display-graphic-p) (fboundp 'context-menu-mode))
    (add-hook 'after-init-hook #'context-menu-mode))

;; Wrapping
;; Continue wrapped lines at whitespace rather than breaking in the middle of a word.
(setq-default word-wrap t)
;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)
;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setopt truncate-partial-width-windows nil)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines.
(setopt comment-multi-line t)
;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance.
(setopt comment-empty-lines t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Eliminate delay before highlighting search matches
(setopt lazy-highlight-initial-delay 0)

;;; Dired

(setopt dired-free-space nil
        dired-dwim-target t  ; Propose a target for intelligent moving or copying.
        dired-deletion-confirmer 'y-or-n-p
        dired-filter-verbose nil
        dired-recursive-deletes 'top
        dired-recursive-copies 'always
        dired-create-destination-dirs 'ask
        image-dired-thumb-size 150
        dired-vc-rename-file t
        dired-clean-confirm-killing-deleted-buffers nil
        dired-omit-verbose nil
        dired-omit-files (concat "\\`[.]\\'"))

;; Configure Ediff to use a single frame and split windows horizontally
(setopt ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

;;; Eglot
(setopt eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-extend-to-xref t)

;; Performance
;; Reduce command completion overhead.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Prevents help command completion from triggering autoload.
;; Loading additional files for completion can slow down help commands and may
;; unintentionally execute initialization code from some libraries.
(setopt help-enable-completion-autoload nil
        help-enable-autoload nil
        help-enable-symbol-autoload nil
        help-window-select t)  ;; Focus new help windows when opened

;; Enable minibuffer history
(savehist-mode 1)

;; Makes manual buffer switching obey configured display actions
(setopt switch-to-buffer-obey-display-actions t)

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)                 ; Set a minimum width

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Set tabs to spaces
(setq-default indent-tabs-mode nil)
;; Set tab to two spaces
(setq-default tab-width 2)
(setq-default smie-indent-basic 2)
(setq-default c-basic-offset 2)
(setq-default sh-basic-offset 2)
(setq-default standard-indent 2)
(setq-default typescript-indent-level 2)
(setq-default evil-shift-width 2)

;; Force Emacs to use a POSIX shell
(setopt shell-file-name (executable-find "bash"))
;; Switch vterm to fish shell
(setopt vterm-shell (executable-find "fish"))
(setopt explicit-shell-file-name (executable-find "fish"))

;; Magit config
(setopt magit-define-global-key-bindings 'recommended)

;; 1Password config
(setopt auth-source-1password-vault "Private")
(setopt auth-source-1password-construct-secret-reference #'auth-source-1password--1password-construct-query-path-escaped)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9b21c848d09ba7df8af217438797336ac99cbbbc87a08dc879e9291673a6a631"
     "fc1275617f9c8d1c8351df9667d750a8e3da2658077cfdda2ca281a2ebc914e0" default))
 '(helm-minibuffer-history-key "M-p"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
