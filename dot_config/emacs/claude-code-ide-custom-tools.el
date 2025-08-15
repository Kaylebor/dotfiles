;;; claude-code-ide-custom-tools.el --- Custom MCP tools for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: ai, claude, mcp, tools, lsp, emacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides custom MCP tools for Claude Code IDE, exposing
;; LSP functionality and other Emacs capabilities to Claude through
;; the Model Context Protocol.

;;; Code:

(require 'claude-code-ide-mcp-server)
(require 'cl-lib)

;; LSP declarations
(declare-function lsp-mode "lsp-mode")
(declare-function lsp-describe-thing-at-point "lsp-mode")
(declare-function lsp-find-definition "lsp-mode")
(declare-function lsp-find-references "lsp-mode")
(declare-function lsp-request "lsp-mode")
(declare-function lsp--text-document-position-params "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")

;;; LSP Tool Functions

(defun claude-code-custom-lsp-describe-thing-at-point (file-path line column)
  "Get LSP type and documentation information at the specified position.
FILE-PATH specifies the file, LINE is 1-based, COLUMN is 0-based."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (not (bound-and-true-p lsp-mode))
                  (format "LSP mode is not active in %s" file-path)
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column column)
                  (let ((result (lsp-describe-thing-at-point)))
                    (if result
                        result
                      "No LSP information available at this position"))))))
        (error
         (format "Error getting LSP info for %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(defun claude-code-custom-lsp-find-definition (file-path line column identifier)
  "Find definition of IDENTIFIER at the specified position using LSP.
FILE-PATH specifies the file, LINE is 1-based, COLUMN is 0-based."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (not (bound-and-true-p lsp-mode))
                  (format "LSP mode is not active in %s" file-path)
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column column)
                  (condition-case lsp-err
                      (let ((definitions (lsp-request "textDocument/definition"
                                                     (lsp--text-document-position-params))))
                        (if definitions
                            (let ((def-list (cond
                                           ((vectorp definitions) (append definitions nil))
                                           ((listp definitions) definitions)  ; Already a list
                                           (t (list definitions)))))
                              (mapcar (lambda (def)
                                        (let* ((uri (or (plist-get def :targetUri)  ; for locationLink format
                                                       (plist-get def :uri)))       ; for location format
                                               (range (or (plist-get def :targetSelectionRange)  ; for locationLink
                                                         (plist-get def :targetRange)            ; for locationLink
                                                         (plist-get def :range)))                ; for location
                                               (start (when range (plist-get range :start))))
                                          (if (and uri range start)
                                              (format "%s:%d:%d"
                                                      (lsp--uri-to-path uri)
                                                      (1+ (plist-get start :line))
                                                      (plist-get start :character))
                                            (format "Invalid definition data: %S" def))))
                                      def-list))
                          (format "No definition found for '%s'" identifier)))
                    (error
                     (format "LSP error finding definition: %s" (error-message-string lsp-err))))))))
        (error
         (format "Error finding definition for %s at %s:%d:%d: %s"
                 identifier file-path line column (error-message-string err)))))))

(defun claude-code-custom-lsp-find-references (file-path line column identifier)
  "Find all references to IDENTIFIER at the specified position using LSP.
FILE-PATH specifies the file, LINE is 1-based, COLUMN is 0-based."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (not (bound-and-true-p lsp-mode))
                  (format "LSP mode is not active in %s" file-path)
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column column)
                  (condition-case lsp-err
                      (let ((references (lsp-request "textDocument/references"
                                                   (append (lsp--text-document-position-params)
                                                          `(:context (:includeDeclaration t))))))
                        (if (and references (> (length references) 0))
                            (let ((ref-list (cond
                                           ((vectorp references) (append references nil))
                                           ((listp references) references)  ; Already a list
                                           (t (list references)))))
                              (mapcar (lambda (ref)
                                        (let* ((uri (plist-get ref :uri))
                                               (range (plist-get ref :range))
                                               (start (when range (plist-get range :start))))
                                          (if (and uri range start)
                                              (format "%s:%d:%d"
                                                      (lsp--uri-to-path uri)
                                                      (1+ (plist-get start :line))
                                                      (plist-get start :character))
                                            (format "Invalid reference data: %S" ref))))
                                      ref-list))
                          (format "No references found for '%s'" identifier)))
                    (error
                     (format "LSP error finding references: %s" (error-message-string lsp-err))))))))
        (error
         (format "Error finding references for %s at %s:%d:%d: %s"
                 identifier file-path line column (error-message-string err)))))))

(defun claude-code-custom-lsp-hover-info (file-path line column)
  "Get LSP hover information at the specified position.
FILE-PATH specifies the file, LINE is 1-based, COLUMN is 0-based."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (not (bound-and-true-p lsp-mode))
                  (format "LSP mode is not active in %s" file-path)
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column column)
                  (condition-case lsp-err
                      ;; Try hover request directly - let LSP handle capability checking
                      (let ((hover-info (condition-case hover-err
                                          (lsp-request "textDocument/hover"
                                                     (lsp--text-document-position-params))
                                        (error nil))))
                        (if hover-info
                            (let ((contents (plist-get hover-info :contents)))
                              (cond
                               ((stringp contents) contents)
                               ((plist-member contents :value)
                                (plist-get contents :value))
                               ((vectorp contents)
                                (mapconcat (lambda (item)
                                            (if (stringp item)
                                                item
                                              (plist-get item :value)))
                                          contents "\n"))
                               (t (format "Hover info format: %S" contents))))
                          "No hover information available at this position"))
                    (error
                     (format "LSP error getting hover info: %s" (error-message-string lsp-err))))))))
        (error
         (format "Error getting hover info at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

;;; Tool Registration

;;;###autoload
(defun claude-code-ide-custom-tools-setup ()
  "Set up custom MCP tools for Claude Code IDE."
  (interactive)
  
  ;; Ensure MCP server is enabled
  (setq claude-code-ide-enable-mcp-server t)

  ;; Register LSP describe tool
  (claude-code-ide-make-tool
   :function #'claude-code-custom-lsp-describe-thing-at-point
   :name "lsp_describe_thing_at_point"
   :description "Get comprehensive LSP type information and documentation for a symbol at a specific position in a file. This provides the same detailed information you would see in Emacs when hovering over a symbol or using describe-thing-at-point."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze")
           (:name "line"
            :type number
            :description "Line number (1-based)")
           (:name "column"
            :type number
            :description "Column number (0-based)")))

  ;; Register LSP find definition tool
  (claude-code-ide-make-tool
   :function #'claude-code-custom-lsp-find-definition
   :name "lsp_find_definition"
   :description "Find the definition location of a symbol using LSP. This is equivalent to 'Go to Definition' in your IDE and will return the exact file and position where the symbol is defined."
   :args '((:name "file_path"
            :type string
            :description "Path to the file containing the symbol")
           (:name "line"
            :type number
            :description "Line number where the symbol is located (1-based)")
           (:name "column"
            :type number
            :description "Column number where the symbol is located (0-based)")
           (:name "identifier"
            :type string
            :description "The symbol/identifier to find the definition for")))

  ;; Register LSP find references tool
  (claude-code-ide-make-tool
   :function #'claude-code-custom-lsp-find-references
   :name "lsp_find_references"
   :description "Find all references to a symbol throughout the project using LSP. This provides a comprehensive list of everywhere the symbol is used, including the declaration itself."
   :args '((:name "file_path"
            :type string
            :description "Path to the file containing the symbol")
           (:name "line"
            :type number
            :description "Line number where the symbol is located (1-based)")
           (:name "column"
            :type number
            :description "Column number where the symbol is located (0-based)")
           (:name "identifier"
            :type string
            :description "The symbol/identifier to find references for")))

  ;; Register LSP hover info tool
  (claude-code-ide-make-tool
   :function #'claude-code-custom-lsp-hover-info
   :name "lsp_hover_info"
   :description "Get hover documentation for a symbol at a specific position using LSP. This provides quick documentation, type signatures, and other contextual information that would appear in a hover popup."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze")
           (:name "line"
            :type number
            :description "Line number (1-based)")
           (:name "column"
            :type number
            :description "Column number (0-based)")))
  
  (message "Custom LSP MCP tools registered successfully"))

(provide 'claude-code-ide-custom-tools)
;;; claude-code-ide-custom-tools.el ends here