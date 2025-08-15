;;; claude-code-ide-mcp-macros.el --- MCP tool registration macros -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: ai, claude, mcp, tools, macros

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides the macro system for automatically registering MCP tools
;; with Claude Code IDE. The `define-mcp-tool` macro combines function definition
;; with automatic registration, reducing boilerplate and ensuring consistency.

;;; Code:

(defvar claude-code-mcp-tools nil
  "List of MCP tools to be registered.
Each entry is a plist with :name, :function, :description, and :args.")

;; Buffer management utility
(defun claude-code-with-temp-buffer (file-path func)
  "Execute FUNC with buffer for FILE-PATH, managing buffer lifecycle.
Tries to use existing buffer first, creates temporary one if needed,
and cleans up temporary buffers afterward."
  (let* ((existing-buffer (find-buffer-visiting file-path))
         (target-buffer (or existing-buffer (find-file-noselect file-path)))
         (was-temp (not existing-buffer)))
    (unwind-protect
        (with-current-buffer target-buffer
          (funcall func))
      ;; Clean up temporary buffer if we created it
      (when (and was-temp (buffer-live-p target-buffer))
        (kill-buffer target-buffer)))))

(defmacro define-mcp-tool (name args docstring &rest body)
  "Define an MCP tool function and automatically register it.

NAME is the tool name (symbol, without claude-code-custom- prefix).
ARGS is the argument list for the function.
DOCSTRING includes the tool description (first paragraph used for MCP).
BODY is the function implementation.

The macro creates a function named claude-code-custom-NAME and automatically
registers it as an MCP tool with name NAME (hyphens converted to underscores).
Automatically handles type conversion for common numeric parameters.

Example:
  (define-mcp-tool goto-line-column (file-path line column)
    \"Move cursor to a specific line and column.\"
    ;; line and column are automatically converted to numbers if strings
    (progn ...))"
  (declare (indent 3) (doc-string 3))
  (let* ((func-name (intern (format "claude-code-custom-%s" name)))
         (tool-name (replace-regexp-in-string "-" "_" (symbol-name name)))
         ;; Extract first paragraph of docstring as description
         (description (car (split-string docstring "\n\n" t)))
         ;; Parse args to create MCP argument specifications
         (mcp-args (claude-code--parse-mcp-args args docstring))
         ;; Detect numeric parameters that need automatic conversion
         (has-line (memq 'line args))
         (has-column (memq 'column args))
         (has-max-count (memq 'max-count args))
         (has-limit (memq 'limit args))
         (has-start-line (memq 'start-line args))
         (has-end-line (memq 'end-line args))
         ;; Build conversion bindings for numeric parameters
         (conversions
          (append
           (when has-line 
             '((line (if (stringp line) (string-to-number line) line))))
           (when has-column 
             '((column (and column (if (stringp column) (string-to-number column) column)))))
           (when has-max-count
             '((max-count (and max-count (if (stringp max-count) (string-to-number max-count) max-count)))))
           (when has-limit
             '((limit (and limit (if (stringp limit) (string-to-number limit) limit)))))
           (when has-start-line
             '((start-line (if (stringp start-line) (string-to-number start-line) start-line))))
           (when has-end-line
             '((end-line (if (stringp end-line) (string-to-number end-line) end-line))))))
         ;; Wrap body with conversions if needed
         (wrapped-body
          (if conversions
              `((let ,conversions ,@body))
            body)))
    `(progn
       ;; Define the function with automatic type conversions
       (defun ,func-name ,args
         ,docstring
         ,@wrapped-body)
       ;; Add to registration list
       (add-to-list 'claude-code-mcp-tools
                    (list :name ,tool-name
                          :function #',func-name
                          :description ,description
                          :args ',mcp-args)))))

(defun claude-code--parse-mcp-args (args docstring)
  "Parse function ARGS and DOCSTRING to create MCP argument specifications."
  (let ((mcp-args '()))
    (dolist (arg args)
      (when (and (symbolp arg) 
                 (not (memq arg '(&optional &rest))))
        (let ((arg-name (symbol-name arg))
              (arg-desc (claude-code--extract-arg-description arg docstring))
              (optional (claude-code--arg-is-optional-p arg args)))
          (push `(:name ,arg-name
                  :type string
                  ,@(when optional '(:optional t))
                  :description ,arg-desc)
                mcp-args))))
    (nreverse mcp-args)))

(defun claude-code--extract-arg-description (arg docstring)
  "Extract description for ARG from DOCSTRING.
Looks for patterns like \\='ARG - description\\=' or \\='ARG: description\\='."
  (let ((arg-name (upcase (symbol-name arg))))
    (cond
     ;; Look for "ARG - description" pattern
     ((string-match (format "%s[[:space:]]*-[[:space:]]*\\([^\n.]+\\)" arg-name) docstring)
      (match-string 1 docstring))
     ;; Look for "ARG: description" pattern  
     ((string-match (format "%s[[:space:]]*:[[:space:]]*\\([^\n.]+\\)" arg-name) docstring)
      (match-string 1 docstring))
     ;; Default description
     (t (format "The %s parameter" (symbol-name arg))))))

(defun claude-code--arg-is-optional-p (arg args)
  "Check if ARG appears after &optional in ARGS list."
  (let ((found-optional nil)
        (is-optional nil))
    (dolist (a args)
      (cond
       ((eq a '&optional) (setq found-optional t))
       ((eq a '&rest) (setq found-optional nil))
       ((and found-optional (eq a arg)) (setq is-optional t))))
    is-optional))

(provide 'claude-code-ide-mcp-macros)
;;; claude-code-ide-mcp-macros.el ends here