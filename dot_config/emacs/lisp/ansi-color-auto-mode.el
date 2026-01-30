;;; ansi-color-auto-mode.el --- Toggle ANSI color rendering automatically -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; URL: https://github.com/yourusername/ansi-color-auto-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, processes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Automatically enable ANSI color rendering in appropriate buffers.
;; Provides a toggle-able minor mode for both live process output and
;; static log files.

;; Features:
;; - Minor mode (`ansi-color-auto-mode') for buffer-local toggle
;; - Global mode (`global-ansi-color-auto-mode') for automatic enable
;; - Supports: compilation, shell, eshell, and log files
;; - Respects detached.el (skips detached session buffers)
;; - Size limit configuration for large files
;; - Overlay-based toggle for static buffers

;;; Code:

(require 'ansi-color)
(require 'cl-lib)

;;;; Customization

(defgroup ansi-color-auto-mode nil
  "Automatically toggle ANSI color rendering in buffers."
  :group 'faces
  :prefix "ansi-color-auto-mode-")

(defcustom ansi-color-auto-mode-size-limit nil
  "Maximum file size to process ANSI colors.
Nil means no limit.  Supports human-readable suffixes:
K/M/G/T (1000-based) or Ki/Mi/Gi/Ti (1024-based).
Examples: \"5M\" = 5,000,000 bytes, \"1Gi\" = 1,073,741,824 bytes."
  :type '(choice (const :tag "No limit" nil)
                 (string :tag "Size with suffix (e.g., 5M, 1Gi)"))
  :group 'ansi-color-auto-mode)

(defcustom ansi-color-auto-mode-global-modes
  '(compilation-mode shell-mode eshell-mode)
  "Major modes where `global-ansi-color-auto-mode' should enable.
Each entry can be a symbol or a cons cell (MODE . FN) where FN is a
function to call when enabling the mode."
  :type '(repeat (choice symbol
                        (cons symbol function)))
  :group 'ansi-color-auto-mode)

(defcustom ansi-color-auto-mode-file-patterns
  '("\\.log\\'" "-log\\'")
  "File name patterns to enable `ansi-color-auto-mode'.
Each pattern is a regexp matched against the buffer file name."
  :type '(repeat string)
  :group 'ansi-color-auto-mode)

(defcustom ansi-color-auto-mode-buffer-name-patterns
  '("\\*Async Shell Command\\*")
  "Buffer name patterns to enable `ansi-color-auto-mode'.
Each pattern is a regexp matched against the buffer name."
  :type '(repeat string)
  :group 'ansi-color-auto-mode)

;;;; Internal Variables

(defvar-local ansi-color-auto-mode--overlays nil
  "Overlays created by `ansi-color-auto-mode' for static buffers.
Used to track and remove color rendering when toggling off.")

(defvar-local ansi-color-auto-mode--original-filter nil
  "Original filter function for live process buffers.
Stored so we can restore it when toggling the mode off.")

;;;; Utility Functions

(defun ansi-color-auto-mode--parse-size (size)
  "Parse SIZE string to bytes.  Returns nil if SIZE is nil.
Supports suffixes: K/M/G/T (decimal, 1000) and Ki/Mi/Gi/Ti (binary, 1024).
Examples: \"5M\" -> 5000000, \"1Gi\" -> 1073741824."
  (when size
    (if (numberp size)
        size
      (when (string-match "^\\s-*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\s-*\\([kmgtpe]?i?\\)\\s-*$"
                          (downcase size))
        (let ((num (string-to-number (match-string 1 size)))
              (suffix (match-string 2 size)))
          (floor (* num (pcase suffix
                          ("" 1)
                          ("k" 1000) ("ki" 1024)
                          ("m" 1000000) ("mi" 1048576)
                          ("g" 1000000000) ("gi" 1073741824)
                          ("t" 1000000000000) ("ti" 1099511627776)
                          ("p" 1000000000000000) ("pi" 1125899906842624)
                          ("e" 1000000000000000000) ("ei" 1152921504606846976)
                          (_ 1)))))))))

(defun ansi-color-auto-mode--file-size-exceeds-limit-p ()
  "Return t if current buffer's file exceeds `ansi-color-auto-mode-size-limit'."
  (when (and ansi-color-auto-mode-size-limit
             buffer-file-name
             (file-exists-p buffer-file-name))
    (let ((limit (ansi-color-auto-mode--parse-size ansi-color-auto-mode-size-limit))
          (size (file-attribute-size (file-attributes buffer-file-name))))
      (and limit size (> size limit)))))

(defun ansi-color-auto-mode--detached-buffer-p ()
  "Return t if current buffer is managed by detached.el."
  (bound-and-true-p detached-buffer-session))

(defun ansi-color-auto-mode--should-enable-p ()
  "Return t if `ansi-color-auto-mode' should be enabled in current buffer.
Checks for detached.el, size limits, and buffer patterns."
  (and (not (ansi-color-auto-mode--detached-buffer-p))
       (not (ansi-color-auto-mode--file-size-exceeds-limit-p))
       (or (cl-some (lambda (pattern)
                      (and buffer-file-name
                           (string-match-p pattern buffer-file-name)))
                    ansi-color-auto-mode-file-patterns)
           (cl-some (lambda (pattern)
                      (let ((bufname (buffer-name)))
                        (and bufname (string-match-p pattern bufname))))
                    ansi-color-auto-mode-buffer-name-patterns)
           (cl-some (lambda (mode-entry)
                      (let ((mode (if (consp mode-entry) (car mode-entry) mode-entry)))
                        (derived-mode-p mode)))
                    ansi-color-auto-mode-global-modes))))

;;;; Buffer Type Detection

(defun ansi-color-auto-mode--live-process-buffer-p ()
  "Return t if current buffer has a live process (comint, compilation, eshell)."
  (or (and (derived-mode-p 'comint-mode) (get-buffer-process (current-buffer)))
      (and (derived-mode-p 'compilation-mode) compilation-in-progress)
      (derived-mode-p 'eshell-mode)))

(defun ansi-color-auto-mode--static-buffer-p ()
  "Return t if current buffer is static (file, async command output, etc.)."
  (not (ansi-color-auto-mode--live-process-buffer-p)))

;;;; Enable/Disable Functions

(defun ansi-color-auto-mode--enable-live ()
  "Enable ANSI color processing for live process buffers."
  (cond
   ((derived-mode-p 'compilation-mode)
    (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))
   ((derived-mode-p 'comint-mode)
    (add-hook 'comint-preoutput-filter-functions #'ansi-color-process-output nil t))
   ((derived-mode-p 'eshell-mode)
    (add-to-list 'eshell-output-filter-functions 'ansi-color-process-output t))))

(defun ansi-color-auto-mode--disable-live ()
  "Disable ANSI color processing for live process buffers."
  (cond
   ((derived-mode-p 'compilation-mode)
    (remove-hook 'compilation-filter-hook #'ansi-color-compilation-filter t))
   ((derived-mode-p 'comint-mode)
    (remove-hook 'comint-preoutput-filter-functions #'ansi-color-process-output t))
   ((derived-mode-p 'eshell-mode)
    (setq eshell-output-filter-functions
          (remove 'ansi-color-process-output eshell-output-filter-functions)))))

(defun ansi-color-auto-mode--enable-static ()
  "Enable ANSI color processing for static buffers using overlays."
  (let ((buffer-read-only buffer-read-only)
        (existing-overlays (overlays-in (point-min) (point-max))))
    (setq-local buffer-read-only nil)
    (ansi-color-apply-on-region (point-min) (point-max) t)
    ;; Track NEW overlays created by ansi-color
    (dolist (ov (overlays-in (point-min) (point-max)))
      (unless (memq ov existing-overlays)
        (push ov ansi-color-auto-mode--overlays)))
    (setq-local buffer-read-only t)))

(defun ansi-color-auto-mode--disable-static ()
  "Disable ANSI color processing for static buffers by removing overlays."
  (mapc #'delete-overlay ansi-color-auto-mode--overlays)
  (setq-local ansi-color-auto-mode--overlays nil))

;;;; Minor Mode Definition

;;;###autoload
(define-minor-mode ansi-color-auto-mode
  "Toggle ANSI color rendering in the current buffer.
When enabled, ANSI escape sequences are rendered as colors.
When disabled, raw ANSI codes are visible.

This mode automatically detects buffer type:
- Live process buffers (shell, compilation, eshell): filters output as it arrives
- Static buffers (log files, async output): applies colors via overlays that can be removed"
  :lighter " ANSI-Auto"
  :group 'ansi-color-auto-mode
  (if ansi-color-auto-mode
      (if (ansi-color-auto-mode--live-process-buffer-p)
          (ansi-color-auto-mode--enable-live)
        (ansi-color-auto-mode--enable-static))
    (if (ansi-color-auto-mode--live-process-buffer-p)
        (ansi-color-auto-mode--disable-live)
      (ansi-color-auto-mode--disable-static))))

;;;; Global Mode

(defun ansi-color-auto-mode--turn-on-maybe ()
  "Turn on `ansi-color-auto-mode' if appropriate for current buffer."
  (when (ansi-color-auto-mode--should-enable-p)
    (ansi-color-auto-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-ansi-color-auto-mode
  ansi-color-auto-mode ansi-color-auto-mode--turn-on-maybe
  :group 'ansi-color-auto-mode)

;;;; Provide

(provide 'ansi-color-auto-mode)

;;; ansi-color-auto-mode.el ends here
