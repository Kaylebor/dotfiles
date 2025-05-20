;; -*- lexical-binding: t; -*-

;; Always precompile packages on install instead of on load
(setq-default package-native-compile t)
(setq-default load-prefer-newer t)
(setq-default native-comp-deferred-compilation t)
(setq-default native-comp-jit-compilation t)

;; Increase garbage collection threshold
(setq gc-cons-threshold 100000000)

;; For lsp-mode: use plists
(setenv "LSP_USE_PLISTS" "true")

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb

(setq-default process-adaptive-read-buffering nil)

(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(when (and (not (daemonp)) (not noninteractive))
  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)
  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)
  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)
  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  ;; Shave seconds off startup time by starting the scratch buffer in
  ;; `fundamental-mode'
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)
  ;; Unset command line options irrelevant to the current OS. These options
  ;; are still processed by `command-line-1` but have no effect.
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

;;; Performance: Disable mode-line during startup
(when (and (not (daemonp))
           (not noninteractive))
  (put 'mode-line-format
       'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format nil))))

;;; Security
(setq gnutls-verify-error t)  ; Prompts user if there are certificate issues
(setq tls-checktrust t)  ; Ensure SSL/TLS connections undergo trust verification
(setq gnutls-min-prime-bits 3072)  ; Stronger GnuTLS encryption

;;; packages
(setq package-enable-at-startup nil) ;; Disable package startup as we will use elpaca instead

;;; UI
(setq ns-use-proxy-icon nil)         ;; Remove icon from title bar

(setq frame-resize-pixelwise t)      ;; Whether a frame can be resized by pixel
(tool-bar-mode -1)                   ;; Disable the toolbar

(setq default-frame-alist '((width . (text-pixels . 800))
                            (height . (text-pixels . 600))
                            (font . "Iosevka Nerd Font-10:weight=semi-light")
                            ;; You can turn off scroll bars by uncommenting these lines:
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)

                            ;; Set as same values from Catppuccin theme to avoid flashes of color during theme activation
                            (background-color . "#303446")
                            (foreground-color . "#c6d0f5")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
