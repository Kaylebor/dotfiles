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
;; The tools are organized into five categories:
;; 1. LSP Tools - Language server integration for definitions, references, hover
;; 2. Project Navigation & Search - File finding and text search capabilities  
;; 3. Diagnostics & Error Analysis - Flycheck errors and compilation results
;; 4. TreeSitter Analysis - Syntax tree analysis and code structure
;; 5. Git/Version Control - Git status, diffs, blame, and file history
;; 6. Code Intelligence - Symbol listing, pattern search, and function context
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

;; LSP declarations
(declare-function lsp-mode "lsp-mode")
(declare-function lsp-describe-thing-at-point "lsp-mode")
(declare-function lsp-find-definition "lsp-mode")
(declare-function lsp-find-references "lsp-mode")
(declare-function lsp-request "lsp-mode")
(declare-function lsp--text-document-position-params "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")

;; Project and search tool declarations
(declare-function projectile-find-file "projectile")
(declare-function projectile-project-root "projectile") 
(declare-function projectile-grep "projectile")
(declare-function rg-project "rg")
(declare-function consult-grep "consult")

;; Diagnostics declarations
(declare-function flycheck-list-errors "flycheck")
(declare-function flycheck-error-message "flycheck")
(declare-function flycheck-next-error "flycheck")
(declare-function compile "compile")

;; TreeSitter declarations
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-children "treesit")

;; Git/Magit declarations
(declare-function magit-status "magit")
(declare-function magit-diff "magit")
(declare-function magit-blame-at-point "magit-blame")
(declare-function magit-log-buffer-file "magit-log")

;; Code intelligence declarations
(declare-function imenu--make-index-alist "imenu")
(declare-function occur "replace")
(declare-function xref-find-apropos "xref")
(declare-function which-function "which-func")

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

;;; Project Navigation & Search Tool Functions

(defun claude-code-custom-projectile-find-file (pattern)
  "Find files in project matching PATTERN using projectile."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (if (not (bound-and-true-p projectile-mode))
            "Projectile mode is not active"
          (let* ((project-root (projectile-project-root))
                 (project-files (projectile-current-project-files))
                 (matches (cl-remove-if-not 
                          (lambda (file) 
                            (string-match-p pattern file))
                          project-files)))
            (if matches
                (mapcar (lambda (file)
                          (format "%s/%s" project-root file))
                        (cl-subseq matches 0 (min 20 (length matches))))
              (format "No files found matching pattern '%s'" pattern))))
      (error
       (format "Error finding files with pattern '%s': %s" 
               pattern (error-message-string err))))))

(defun claude-code-custom-projectile-grep (pattern)
  "Search for PATTERN across project files using projectile."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (if (not (bound-and-true-p projectile-mode))
            "Projectile mode is not active"
          (let* ((project-root (projectile-project-root))
                 (default-directory project-root)
                 (results (shell-command-to-string
                          (format "rg -n --max-count 50 '%s' ." pattern))))
            (if (string-empty-p (string-trim results))
                (format "No matches found for pattern '%s'" pattern)
              results)))
      (error
       (format "Error searching for pattern '%s': %s" 
               pattern (error-message-string err))))))

(defun claude-code-custom-rg-search (pattern file-types)
  "Advanced ripgrep search for PATTERN with optional FILE-TYPES filter."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((type-mapping '(("rb" . "ruby") ("py" . "py") ("js" . "js") 
                              ("ts" . "ts") ("go" . "go") ("rs" . "rust")
                              ("erb" . "ruby") ("haml" . "ruby") ("slim" . "ruby")
                              ("jsx" . "js") ("vue" . "js") ("svelte" . "js")
                              ("tsx" . "ts") ("html" . "html") ("htm" . "html")
                              ("tmpl" . "go") ("gotmpl" . "go")))
               (mapped-type (or (cdr (assoc file-types type-mapping)) file-types))
               (type-arg (if mapped-type (format "-t %s" mapped-type) ""))
               (cmd (format "rg -n --max-count 50 %s '%s' ." type-arg pattern))
               (results (shell-command-to-string cmd)))
          (if (string-empty-p (string-trim results))
              (format "No matches found for pattern '%s'" pattern)
            results))
      (error
       (format "Error in rg search for pattern '%s': %s" 
               pattern (error-message-string err))))))

(defun claude-code-custom-consult-grep (pattern)
  "Interactive grep using consult for PATTERN, returns top matches."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((default-directory (or (projectile-project-root) default-directory))
               (cmd (format "rg -n --max-count 20 '%s' ." pattern))
               (results (shell-command-to-string cmd)))
          (if (string-empty-p (string-trim results))
              (format "No matches found for pattern '%s'" pattern)
            (format "Grep results for '%s':\n%s" pattern results)))
      (error
       (format "Error in consult grep for pattern '%s': %s" 
               pattern (error-message-string err))))))

;;; Diagnostics & Error Analysis Tool Functions

(defun claude-code-custom-flycheck-list-errors (file-path)
  "List all flycheck errors and warnings in FILE-PATH."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (not (bound-and-true-p flycheck-mode))
                  (format "Flycheck mode is not active in %s" file-path)
                (if flycheck-current-errors
                    (mapcar (lambda (err)
                              (format "%s:%d:%d: %s - %s"
                                      file-path
                                      (flycheck-error-line err)
                                      (or (flycheck-error-column err) 0)
                                      (flycheck-error-level err)
                                      (flycheck-error-message err)))
                            flycheck-current-errors)
                  (format "No flycheck errors found in %s" file-path)))))
        (error
         (format "Error listing flycheck errors for %s: %s"
                 file-path (error-message-string err)))))))

(defun claude-code-custom-flycheck-explain-error (file-path line column)
  "Get detailed explanation of flycheck error at position in FILE-PATH."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (not (bound-and-true-p flycheck-mode))
                  (format "Flycheck mode is not active in %s" file-path)
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column (or column 0))
                  (let ((error-at-point (flycheck-overlay-errors-at (point))))
                    (if error-at-point
                        (let ((err (car error-at-point)))
                          (format "Error: %s\nLevel: %s\nChecker: %s\nMessage: %s"
                                  (flycheck-error-id err)
                                  (flycheck-error-level err)
                                  (flycheck-error-checker err)
                                  (flycheck-error-message err)))
                      (format "No flycheck error at %s:%d:%d" file-path line column)))))))
        (error
         (format "Error explaining flycheck error at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(defun claude-code-custom-compile-project (command)
  "Run compilation COMMAND and return results."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((default-directory (or (projectile-project-root) default-directory))
               (compilation-buffer (compile command)))
          (with-current-buffer compilation-buffer
            ;; Wait a moment for compilation to start
            (sit-for 0.5)
            (if (get-buffer-process compilation-buffer)
                "Compilation started. Check *compilation* buffer for results."
              (buffer-string))))
      (error
       (format "Error running compilation command '%s': %s"
               command (error-message-string err))))))

(defun claude-code-custom-next-error-location ()
  "Find the location of the next compilation or flycheck error."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (save-excursion
          (next-error-find-buffer)
          (let* ((next-error-buffer (next-error-find-buffer))
                 (next-error-function (if next-error-buffer
                                        (buffer-local-value 'next-error-function
                                                           next-error-buffer))))
            (if next-error-function
                (with-current-buffer next-error-buffer
                  (funcall next-error-function 1 nil)
                  (format "Next error location: %s" (point-marker)))
              "No error navigation available")))
      (error
       (format "Error finding next error: %s" (error-message-string err))))))

;;; TreeSitter Analysis Tool Functions

(defun claude-code-custom-treesit-analyze-structure (file-path)
  "Get syntax tree structure for FILE-PATH using TreeSitter."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (if (not (treesit-available-p))
              "Tree-sitter is not available in this Emacs build"
            (let ((target-buffer (or (find-buffer-visiting file-path)
                                     (find-file-noselect file-path))))
              (with-current-buffer target-buffer
                (let* ((parsers (treesit-parser-list))
                       (parser (car parsers)))
                  (if (not parser)
                      (format "No tree-sitter parser available for %s" file-path)
                    (let ((root-node (treesit-parser-root-node parser)))
                      (claude-code-ide-mcp-treesit--format-tree root-node 0 15)))))))
        (error
         (format "Error analyzing structure for %s: %s"
                 file-path (error-message-string err)))))))

(defun claude-code-custom-treesit-get-node-at-point (file-path line column)
  "Get TreeSitter node information at specific position."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (if (not (treesit-available-p))
              "Tree-sitter is not available in this Emacs build"
            (let ((target-buffer (or (find-buffer-visiting file-path)
                                     (find-file-noselect file-path))))
              (with-current-buffer target-buffer
                (let* ((parsers (treesit-parser-list))
                       (parser (car parsers)))
                  (if (not parser)
                      (format "No tree-sitter parser available for %s" file-path)
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (move-to-column (or column 0))
                      (let ((node (treesit-node-at (point) parser)))
                        (if node
                            (format "Node: %s\nRange: %d-%d\nNamed: %s\nText: %s"
                                    (treesit-node-type node)
                                    (treesit-node-start node)
                                    (treesit-node-end node)
                                    (treesit-node-check node 'named)
                                    (truncate-string-to-width
                                     (treesit-node-text node t) 100 nil nil "..."))
                          "No TreeSitter node found at position"))))))))
        (error
         (format "Error getting TreeSitter node at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(defun claude-code-custom-treesit-find-parent-node (file-path line column node-type)
  "Find parent node of specified NODE-TYPE at position."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (if (not (treesit-available-p))
              "Tree-sitter is not available in this Emacs build"
            (let ((target-buffer (or (find-buffer-visiting file-path)
                                     (find-file-noselect file-path))))
              (with-current-buffer target-buffer
                (let* ((parsers (treesit-parser-list))
                       (parser (car parsers)))
                  (if (not parser)
                      (format "No tree-sitter parser available for %s" file-path)
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (move-to-column (or column 0))
                      (let* ((node (treesit-node-at (point) parser))
                             (parent (treesit-parent-until node (lambda (n) 
                                                                 (equal (treesit-node-type n) node-type)))))
                        (if parent
                            (format "Found parent %s at %d-%d\nText: %s"
                                    (treesit-node-type parent)
                                    (treesit-node-start parent)
                                    (treesit-node-end parent)
                                    (truncate-string-to-width
                                     (treesit-node-text parent t) 100 nil nil "..."))
                          (format "No parent node of type '%s' found" node-type)))))))))
        (error
         (format "Error finding parent node at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(defun claude-code-custom-treesit-get-children (file-path line column)
  "Get child nodes at position in FILE-PATH."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (if (not (treesit-available-p))
              "Tree-sitter is not available in this Emacs build"
            (let ((target-buffer (or (find-buffer-visiting file-path)
                                     (find-file-noselect file-path))))
              (with-current-buffer target-buffer
                (let* ((parsers (treesit-parser-list))
                       (parser (car parsers)))
                  (if (not parser)
                      (format "No tree-sitter parser available for %s" file-path)
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (move-to-column (or column 0))
                      (let* ((node (treesit-node-at (point) parser))
                             (child-count (treesit-node-child-count node)))
                        (if (= child-count 0)
                            "Node has no children"
                          (let ((children '()))
                            (dotimes (i (min child-count 10))
                              (let ((child (treesit-node-child node i)))
                                (when child
                                  (push (format "[%d] %s (%d-%d)"
                                                i
                                                (treesit-node-type child)
                                                (treesit-node-start child)
                                                (treesit-node-end child))
                                        children))))
                            (string-join (nreverse children) "\n"))))))))))
        (error
         (format "Error getting children at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

;;; Git/Version Control Tool Functions

(defun claude-code-custom-magit-status-summary ()
  "Get git status summary for current project."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((default-directory (or (projectile-project-root) default-directory))
               (status-output (shell-command-to-string "git status --porcelain")))
          (if (string-empty-p (string-trim status-output))
              "Working directory clean"
            (format "Git status:\n%s" status-output)))
      (error
       (format "Error getting git status: %s" (error-message-string err))))))

(defun claude-code-custom-magit-diff-summary (staged)
  "Get git diff summary. If STAGED is non-nil, show staged changes."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((default-directory (or (projectile-project-root) default-directory))
               (diff-cmd (if staged "git diff --cached" "git diff"))
               (diff-output (shell-command-to-string diff-cmd)))
          (if (string-empty-p (string-trim diff-output))
              (if staged "No staged changes" "No unstaged changes")
            (format "%s diff:\n%s" 
                    (if staged "Staged" "Unstaged")
                    (truncate-string-to-width diff-output 2000 nil nil "..."))))
      (error
       (format "Error getting git diff: %s" (error-message-string err))))))

(defun claude-code-custom-magit-blame-at-line (file-path line)
  "Get git blame information for LINE in FILE-PATH."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((default-directory (or (projectile-project-root) default-directory))
                 (blame-cmd (format "git blame -L %d,%d %s" line line file-path))
                 (blame-output (shell-command-to-string blame-cmd)))
            (if (string-empty-p (string-trim blame-output))
                (format "No git blame information for %s:%d" file-path line)
              (format "Git blame for %s:%d:\n%s" file-path line blame-output)))
        (error
         (format "Error getting git blame for %s:%d: %s"
                 file-path line (error-message-string err)))))))

(defun claude-code-custom-magit-log-file (file-path max-count)
  "Get commit history for FILE-PATH with MAX-COUNT entries."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((default-directory (or (projectile-project-root) default-directory))
                 (count (or max-count 10))
                 (log-cmd (format "git log --oneline -n %d %s" count file-path))
                 (log-output (shell-command-to-string log-cmd)))
            (if (string-empty-p (string-trim log-output))
                (format "No git history found for %s" file-path)
              (format "Git log for %s (last %d commits):\n%s" 
                      file-path count log-output)))
        (error
         (format "Error getting git log for %s: %s"
                 file-path (error-message-string err)))))))

;;; Code Intelligence Tool Functions

(defun claude-code-custom-imenu-list-all-symbols (file-path)
  "List all symbols in FILE-PATH using imenu."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
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

(defun claude-code-custom-occur-find-pattern (file-path pattern)
  "Find all occurrences of PATTERN in FILE-PATH."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
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

(defun claude-code-custom-xref-find-apropos (pattern file-path)
  "Find symbols matching PATTERN using xref in FILE-PATH context."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
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

(defun claude-code-custom-which-function-at-point (file-path line column)
  "Get the function name at the specified position in FILE-PATH."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
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

  ;; Register project navigation & search tools
  (claude-code-ide-make-tool
   :function #'claude-code-custom-projectile-find-file
   :name "projectile_find_file"
   :description "Find files in project matching a pattern using projectile fuzzy search. Great for quickly locating files when you know part of the filename."
   :args '((:name "pattern"
            :type string
            :description "Pattern to match against filenames")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-projectile-grep
   :name "projectile_grep"
   :description "Search for text patterns across all files in the current project using ripgrep. Returns file locations with line numbers and context."
   :args '((:name "pattern"
            :type string
            :description "Text pattern to search for across project files")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-rg-search
   :name "rg_search"
   :description "Advanced ripgrep search with optional file type filtering. Provides powerful text search capabilities with regex support and file type constraints."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (supports regex)")
           (:name "file_types"
            :type string
            :optional t
            :description "File type filter (e.g., 'py', 'js', 'rb', 'ts', 'go', 'rs', 'erb', 'jsx', 'vue', 'tsx', 'tmpl'). Common extensions and templates are automatically mapped to ripgrep types.")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-consult-grep
   :name "consult_grep"
   :description "Interactive grep search using consult, returning top matches with context. Ideal for exploring search results with live preview."
   :args '((:name "pattern"
            :type string
            :description "Pattern to search for in project files")))

  ;; Register diagnostics & error analysis tools
  (claude-code-ide-make-tool
   :function #'claude-code-custom-flycheck-list-errors
   :name "flycheck_list_errors"
   :description "Get all flycheck errors and warnings in a file. Shows syntax errors, linting issues, and warnings with precise locations."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze for errors")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-flycheck-explain-error
   :name "flycheck_explain_error"
   :description "Get detailed explanation of a specific flycheck error at a given position. Provides error context, checker information, and suggested fixes."
   :args '((:name "file_path"
            :type string
            :description "Path to the file containing the error")
           (:name "line"
            :type number
            :description "Line number where the error occurs (1-based)")
           (:name "column"
            :type number
            :optional t
            :description "Column number where the error occurs (0-based)")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-compile-project
   :name "compile_project"
   :description "Run a compilation command for the project and return results. Useful for building projects and checking for compilation errors."
   :args '((:name "command"
            :type string
            :description "Compilation command to run (e.g., 'make', 'npm run build')")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-next-error-location
   :name "next_error_location"
   :description "Find the location of the next compilation or flycheck error. Helps navigate through error lists systematically."
   :args nil)

  ;; Register TreeSitter analysis tools
  (claude-code-ide-make-tool
   :function #'claude-code-custom-treesit-analyze-structure
   :name "treesit_analyze_structure"
   :description "Get the complete syntax tree structure for a file using TreeSitter. Provides deep insights into code structure and AST analysis."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze structurally")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-treesit-get-node-at-point
   :name "treesit_get_node_at_point"
   :description "Get detailed TreeSitter node information at a specific position. Shows node type, range, and text for precise syntax analysis."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze")
           (:name "line"
            :type number
            :description "Line number (1-based)")
           (:name "column"
            :type number
            :description "Column number (0-based)")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-treesit-find-parent-node
   :name "treesit_find_parent_node"
   :description "Find the nearest parent node of a specific type in the syntax tree. Useful for understanding code context and structure."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze")
           (:name "line"
            :type number
            :description "Line number (1-based)")
           (:name "column"
            :type number
            :description "Column number (0-based)")
           (:name "node_type"
            :type string
            :description "Type of parent node to find (e.g., 'function_definition', 'class_declaration')")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-treesit-get-children
   :name "treesit_get_children"
   :description "Get all child nodes at a specific position in the syntax tree. Shows the immediate descendants of a syntax node."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze")
           (:name "line"
            :type number
            :description "Line number (1-based)")
           (:name "column"
            :type number
            :description "Column number (0-based)")))

  ;; Register Git/version control tools
  (claude-code-ide-make-tool
   :function #'claude-code-custom-magit-status-summary
   :name "magit_status_summary"
   :description "Get a summary of the current git status for the project. Shows modified, staged, and untracked files."
   :args nil)

  (claude-code-ide-make-tool
   :function #'claude-code-custom-magit-diff-summary
   :name "magit_diff_summary"
   :description "Get git diff summary showing changes in the working directory or staging area. Helps review modifications before committing."
   :args '((:name "staged"
            :type boolean
            :optional t
            :description "If true, show staged changes; if false or omitted, show unstaged changes")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-magit-blame-at-line
   :name "magit_blame_at_line"
   :description "Get git blame information for a specific line. Shows who last modified the line and when, with commit information."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to blame")
           (:name "line"
            :type number
            :description "Line number to get blame info for (1-based)")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-magit-log-file
   :name "magit_log_file"
   :description "Get commit history for a specific file. Shows the chronological list of changes made to the file."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to get history for")
           (:name "max_count"
            :type number
            :optional t
            :description "Maximum number of commits to show (default: 10)")))

  ;; Register code intelligence tools
  (claude-code-ide-make-tool
   :function #'claude-code-custom-imenu-list-all-symbols
   :name "imenu_list_all_symbols"
   :description "List all symbols (functions, classes, variables) in a file using imenu. Provides a comprehensive overview of file structure."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze for symbols")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-occur-find-pattern
   :name "occur_find_pattern"
   :description "Find all occurrences of a pattern within a specific file. Shows line numbers and context for each match."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to search in")
           (:name "pattern"
            :type string
            :description "Pattern to search for (supports regex)")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-xref-find-apropos
   :name "xref_find_apropos"
   :description "Find symbols matching a pattern using xref across the project. Leverages language server or tags for intelligent symbol search."
   :args '((:name "pattern"
            :type string
            :description "Pattern to match against symbol names")
           (:name "file_path"
            :type string
            :description "File path to use as context for the search")))

  (claude-code-ide-make-tool
   :function #'claude-code-custom-which-function-at-point
   :name "which_function_at_point"
   :description "Get the name of the function or method at a specific position. Useful for understanding code context."
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze")
           (:name "line"
            :type number
            :description "Line number (1-based)")
           (:name "column"
            :type number
            :optional t
            :description "Column number (0-based)")))
  
  (message "All custom MCP tools registered successfully"))

(provide 'claude-code-ide-custom-tools)
;;; claude-code-ide-custom-tools.el ends here