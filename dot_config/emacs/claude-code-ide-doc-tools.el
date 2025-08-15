;;; claude-code-ide-doc-tools.el --- Documentation and help tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: ai, claude, mcp, documentation, help

;;; Commentary:

;; Documentation and help tools for Claude Code IDE.
;; Provides access to Emacs function/variable documentation and help systems.

;;; Code:

(require 'claude-code-ide-mcp-macros)

;; Documentation & help declarations
(declare-function describe-function "help-fns")
(declare-function describe-variable "help-fns")
(declare-function apropos-command "apropos")
(declare-function which-key-show-keymap "which-key")

;; Code intelligence declarations
(declare-function imenu--make-index-alist "imenu")
(declare-function occur "replace")
(declare-function xref-find-apropos "xref")
(declare-function which-function "which-func")

(define-mcp-tool describe-function (function-name)
  "Get comprehensive documentation for an Emacs Lisp function.
  
  FUNCTION-NAME - Name of the function to describe
  
  Includes usage examples, parameters, and detailed documentation."
  (progn
    (condition-case err
        (if (fboundp (intern function-name))
            (with-temp-buffer
              (describe-function (intern function-name))
              (with-current-buffer "*Help*"
                (buffer-string)))
          (format "Function '%s' not found" function-name))
      (error
       (format "Error describing function '%s': %s"
               function-name (error-message-string err))))))

(define-mcp-tool describe-variable (variable-name)
  "Get documentation for an Emacs Lisp variable.
  
  VARIABLE-NAME - Name of the variable to describe
  
  Shows current value, purpose, and configuration options."
  (progn
    (condition-case err
        (if (boundp (intern variable-name))
            (with-temp-buffer
              (describe-variable (intern variable-name))
              (with-current-buffer "*Help*"
                (buffer-string)))
          (format "Variable '%s' not found" variable-name))
      (error
       (format "Error describing variable '%s': %s"
               variable-name (error-message-string err))))))

(define-mcp-tool apropos-command (pattern)
  "Find all commands (interactive functions) matching a pattern.
  
  PATTERN - Pattern to search for in command names
  
  Useful for discovering available functionality."
  (progn
    (condition-case err
        (let ((commands (apropos-internal pattern 'commandp)))
          (if commands
              (mapcar #'symbol-name commands)
            (format "No commands found matching pattern '%s'" pattern)))
      (error
       (format "Error finding commands matching '%s': %s"
               pattern (error-message-string err))))))

(define-mcp-tool which-key-help ()
  "Check if which-key mode is active for automatic keybinding help.
  
  Which-key provides contextual keybinding assistance."
  (progn
    (condition-case err
        (if (bound-and-true-p which-key-mode)
            "Which-key mode is active. Available keybindings are shown automatically."
          "Which-key mode is not active")
      (error
       (format "Error checking which-key: %s" (error-message-string err))))))

(define-mcp-tool imenu-list-all-symbols (file-path)
  "List all symbols (functions, classes, variables) in a file using imenu.
  
  FILE-PATH - Path to the file to analyze for symbols
  
  Provides a comprehensive overview of file structure."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (imenu--make-index-alist)
              (if imenu--index-alist
                  (let ((results '()))
                    (dolist (item imenu--index-alist)
                      (cond
                       ((string-match-p "^\\*" (car item)) nil) ; Skip special entries
                       ((markerp (cdr item))
                        (let ((line (line-number-at-pos (marker-position (cdr item)))))
                          (push (format "%s:%d: %s" file-path line (car item)) results)))
                       ((numberp (cdr item))
                        (let ((line (line-number-at-pos (cdr item))))
                          (push (format "%s:%d: %s" file-path line (car item)) results)))
                       ((listp (cdr item))
                        (let ((category (car item)))
                          (dolist (subitem (cdr item))
                            (when (and (consp subitem)
                                       (or (markerp (cdr subitem)) (numberp (cdr subitem))))
                              (let ((line (line-number-at-pos
                                           (if (markerp (cdr subitem))
                                               (marker-position (cdr subitem))
                                             (cdr subitem)))))
                                (push (format "%s:%d: [%s] %s" 
                                              file-path line category (car subitem)) 
                                      results))))))))
                    (if results
                        (nreverse results)
                      (format "No symbols found in %s" file-path)))
                (format "No imenu support for %s" file-path))))
        (error
         (format "Error listing symbols in %s: %s"
                 file-path (error-message-string err)))))))

(define-mcp-tool occur-find-pattern (file-path pattern)
  "Find all occurrences of a pattern within a specific file.
  
  FILE-PATH - Path to the file to search in
  PATTERN - Pattern to search for (supports regex)
  
  Shows line numbers and context for each match."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (save-excursion
                (goto-char (point-min))
                (let ((matches '())
                      (line-num 1))
                  (while (re-search-forward pattern nil t)
                    (let ((line-start (line-beginning-position))
                          (line-end (line-end-position)))
                      (push (format "%s:%d: %s"
                                    file-path
                                    line-num
                                    (buffer-substring-no-properties line-start line-end))
                            matches))
                    (forward-line 1)
                    (setq line-num (line-number-at-pos)))
                  (if matches
                      (nreverse matches)
                    (format "No occurrences of pattern '%s' found in %s" pattern file-path))))))
        (error
         (format "Error finding pattern '%s' in %s: %s"
                 pattern file-path (error-message-string err)))))))

(define-mcp-tool xref-find-apropos (pattern file-path)
  "Find symbols matching a pattern using xref across the project.
  
  PATTERN - Pattern to match against symbol names
  FILE-PATH - File path to use as context for the search
  
  Leverages language server or tags for intelligent symbol search."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (let ((backend (xref-find-backend)))
                (if (not backend)
                    (format "No xref backend available for %s" file-path)
                  (let ((xref-items (xref-backend-apropos backend pattern)))
                    (if xref-items
                        (mapcar (lambda (item)
                                  (let* ((location (xref-item-location item))
                                         (file (xref-location-group location))
                                         (marker (xref-location-marker location))
                                         (line (with-current-buffer (marker-buffer marker)
                                                 (save-excursion
                                                   (goto-char marker)
                                                   (line-number-at-pos))))
                                         (summary (xref-item-summary item)))
                                    (format "%s:%d: %s" file line summary)))
                                xref-items)
                      (format "No symbols found matching pattern '%s'" pattern)))))))
        (error
         (format "Error finding symbols matching '%s' in %s: %s"
                 pattern file-path (error-message-string err)))))))

(define-mcp-tool which-function-at-point (file-path line column)
  "Get the name of the function or method at a specific position.
  
  FILE-PATH - Path to the file to analyze
  LINE - Line number (1-based)
  COLUMN - Column number (0-based, optional)
  
  Useful for understanding code context."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- line))
                (move-to-column (or column 0))
                (require 'which-func)
                (let ((func-name (which-function)))
                  (if func-name
                      (format "Current function: %s" func-name)
                    "Not inside a function")))))
        (error
         (format "Error getting function name at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(provide 'claude-code-ide-doc-tools)
;;; claude-code-ide-doc-tools.el ends here