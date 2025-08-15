;;; claude-code-ide-custom-tools.el --- Custom MCP tools for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: ai, claude, mcp, tools, lsp, emacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides custom MCP tools for Claude Code IDE, exposing
;; LSP functionality and other Emacs capabilities to Claude through
;; the Model Context Protocol.
;;
;; The tools are organized into modular category files:
;; - claude-code-ide-lsp-tools.el - Language server integration
;; - claude-code-ide-project-tools.el - Project navigation & search
;; - claude-code-ide-test-tools.el - Testing framework integration  
;; - claude-code-ide-git-tools.el - Git/version control
;; - claude-code-ide-treesit-tools.el - TreeSitter analysis
;; - claude-code-ide-nav-tools.el - Navigation & cursor management
;; - claude-code-ide-doc-tools.el - Documentation & help
;; - claude-code-ide-edit-tools.el - Code editing & manipulation
;;
;; Each category file uses the macro system defined in claude-code-ide-mcp-macros.el
;; for automatic tool registration.
;;
;; Known Limitations:
;; - LSP hover info may not be available for all language servers (e.g., Ruby LSP)
;; - LSP tools require active lsp-mode in target buffers
;; - xref operations depend on language server capabilities or tag files
;; - TreeSitter tools require treesit-available-p and appropriate parsers
;; - Git tools require project to be in a git repository
;; - Some tools may return fuzzy matches (this is expected behavior)

;;; Code:

(require 'claude-code-ide-mcp-server)
(require 'cl-lib)

;; Ensure the current directory is in load path for our category files
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Load the macro system first
(require 'claude-code-ide-mcp-macros)

;; Load all category files
(require 'claude-code-ide-lsp-tools)
(require 'claude-code-ide-project-tools)
(require 'claude-code-ide-test-tools)
(require 'claude-code-ide-git-tools)
(require 'claude-code-ide-treesit-tools)
(require 'claude-code-ide-nav-tools)
(require 'claude-code-ide-doc-tools)
(require 'claude-code-ide-edit-tools)

;; TreeSitter helper function
(defun claude-code-ide-mcp-treesit--format-tree (node depth max-depth)
  "Format TreeSitter NODE as a tree with DEPTH and MAX-DEPTH limits."
  (when (and node (< depth max-depth))
    (let ((indent (make-string (* depth 2) ?\ ))
          (node-type (treesit-node-type node))
          (start (treesit-node-start node))
          (end (treesit-node-end node)))
      (concat indent 
              (format "%s (%d-%d)" node-type start end)
              (when (< depth (1- max-depth))
                (let ((children (treesit-node-children node)))
                  (when children
                    (concat "\n" 
                            (mapconcat (lambda (child)
                                        (claude-code-ide-mcp-treesit--format-tree 
                                         child (1+ depth) max-depth))
                                      children "\n")))))))))

;;; Tool Registration

;;;###autoload
(defun claude-code-ide-custom-tools-setup ()
  "Set up custom MCP tools for Claude Code IDE using the modular macro system."
  (interactive)
  
  ;; Ensure MCP server is enabled
  (setq claude-code-ide-enable-mcp-server t)
  
  ;; Register all tools from the macro-generated list
  (dolist (tool claude-code-mcp-tools)
    (apply #'claude-code-ide-make-tool tool))
  
  (message "Registered %d custom MCP tools using modular macro system" (length claude-code-mcp-tools)))

;; Auto-setup when the file is loaded (deferred to allow category files to load)
(eval-after-load 'claude-code-ide-edit-tools
  '(claude-code-ide-custom-tools-setup))

(provide 'claude-code-ide-custom-tools)
;;; claude-code-ide-custom-tools.el ends here