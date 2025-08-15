;;; claude-code-ide-lsp-tools.el --- LSP integration tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: ai, claude, mcp, lsp

;;; Commentary:

;; LSP (Language Server Protocol) integration tools for Claude Code IDE.
;; Provides symbol information, definitions, references, and hover documentation.

;;; Code:

(require 'claude-code-ide-mcp-macros)

;; LSP function declarations
(declare-function lsp-describe-thing-at-point "lsp-mode")
(declare-function lsp-find-definition "lsp-mode")
(declare-function lsp-find-references "lsp-mode")
(declare-function lsp-request "lsp-mode")
(declare-function lsp--text-document-position-params "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")
(declare-function lsp-workspace-root "lsp-mode")
(declare-function lsp--uri-to-path "lsp-mode")

(define-mcp-tool lsp-describe-thing-at-point (file-path line column)
  "Get comprehensive LSP type information and documentation for a symbol at a specific position in a file.
  
  FILE-PATH - Path to the file to analyze
  LINE - Line number (1-based)  
  COLUMN - Column number (0-based)
  
  This provides the same detailed information you would see in Emacs when hovering over a symbol or using describe-thing-at-point."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (bound-and-true-p lsp-mode)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or column 0))
                    (let ((info (lsp-describe-thing-at-point)))
                      (format "LSP info at %s:%d:%d:\n%s" file-path line column info)))
                "LSP mode is not active in this buffer")))
        (error
         (format "Error getting LSP info at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(define-mcp-tool lsp-find-definition (file-path line column)
  "Find the definition location of a symbol using LSP.
  
  FILE-PATH - Path to the file containing the symbol
  LINE - Line number (1-based)
  COLUMN - Column number (0-based)
  
  This is equivalent to \\='Go to Definition\\=' in your IDE and will return the 
  exact file and position where the symbol is defined."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (bound-and-true-p lsp-mode)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or column 0))
                    (let ((result (lsp-find-definition)))
                      (if result
                          (format "Definition found: %s" result)
                        "No definition found")))
                "LSP mode is not active in this buffer")))
        (error
         (format "Error finding definition at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(define-mcp-tool lsp-find-references (file-path line column)
  "Find all references to a symbol throughout the project using LSP.
  
  FILE-PATH - Path to the file containing the symbol
  LINE - Line number (1-based)
  COLUMN - Column number (0-based)
  
  This provides a comprehensive list of everywhere the symbol is used, including the declaration itself."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (bound-and-true-p lsp-mode)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or column 0))
                    (let* ((params (lsp--text-document-position-params))
                           (refs-response (lsp-request "textDocument/references" 
                                                      (plist-put params :context '(:includeDeclaration t))))
                           (refs (when refs-response
                                   (mapcar (lambda (ref)
                                             (let* ((uri (plist-get ref :uri))
                                                    (range (plist-get ref :range))
                                                    (start (plist-get range :start))
                                                    (ref-line (1+ (plist-get start :line)))
                                                    (ref-char (plist-get start :character))
                                                    (ref-file (lsp--uri-to-path uri)))
                                               (format "%s:%d:%d" ref-file ref-line ref-char)))
                                           refs-response))))
                      (if refs
                          (format "Found %d references:\n%s" 
                                  (length refs) 
                                  (mapconcat #'identity refs "\n"))
                        "No references found")))
                "LSP mode is not active in this buffer")))
        (error
         (format "Error finding references at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(define-mcp-tool lsp-hover-info (file-path line column)
  "Get hover documentation for a symbol at a specific position using LSP.
  
  FILE-PATH - Path to the file
  LINE - Line number (1-based)
  COLUMN - Column number (0-based)
  
  This provides quick documentation, type signatures, and other contextual information that would appear in a hover popup."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (bound-and-true-p lsp-mode)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or column 0))
                    (let ((params (lsp--text-document-position-params)))
                      (if-let ((hover-info (lsp-request "textDocument/hover" params)))
                          (format "Hover info at %s:%d:%d:\n%s" 
                                  file-path line column hover-info)
                        "No hover information available")))
                "LSP mode is not active in this buffer")))
        (error
         (format "Error getting hover info at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(provide 'claude-code-ide-lsp-tools)
;;; claude-code-ide-lsp-tools.el ends here