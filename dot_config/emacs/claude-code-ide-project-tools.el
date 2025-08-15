;;; claude-code-ide-project-tools.el --- Project management and search tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: ai, claude, mcp, projectile, search

;;; Commentary:

;; Project management and search tools for Claude Code IDE.
;; Includes Projectile integration, ripgrep, consult, and buffer management.

;;; Code:

(require 'claude-code-ide-mcp-macros)

;; Project and search tool declarations
(declare-function projectile-find-file "projectile")
(declare-function projectile-project-root "projectile") 
(declare-function projectile-grep "projectile")
(declare-function projectile-test-project "projectile")
(declare-function projectile-test-command "projectile")
(declare-function projectile-project-type "projectile")
(declare-function projectile-find-test-file "projectile")
(declare-function projectile-test-suffix "projectile")
(declare-function projectile-test-directory "projectile")
(declare-function projectile-compile-project "projectile")
(declare-function rg-project "rg")
(declare-function consult-grep "consult")

(define-mcp-tool projectile-find-file (pattern)
  "Find files in project matching a pattern using projectile fuzzy search.
  
  PATTERN - Pattern to search for in filenames
  
  Great for quickly locating files when you know part of the filename."
  (progn
    (condition-case err
        (if (bound-and-true-p projectile-mode)
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
                (format "No files found matching pattern '%s'" pattern)))
          "Projectile mode is not active")
      (error
       (format "Error searching for files matching '%s': %s"
               pattern (error-message-string err))))))

(define-mcp-tool projectile-grep (pattern)
  "Search for text patterns across all files in the current project using ripgrep.
  
  PATTERN - Text pattern to search for
  
  Returns file locations with line numbers and context."
  (progn
    (condition-case err
        (if (bound-and-true-p projectile-mode)
            (let* ((project-root (projectile-project-root))
                   (default-directory project-root)
                   (results (shell-command-to-string
                            (format "rg -n --max-count 50 '%s' ." pattern))))
              (if (string-empty-p (string-trim results))
                  (format "No matches found for pattern '%s'" pattern)
                results))
          "Projectile mode is not active")
      (error
       (format "Error searching for pattern '%s': %s"
               pattern (error-message-string err))))))

(define-mcp-tool rg-search (pattern &optional file-type)
  "Advanced ripgrep search with optional file type filtering.
  
  PATTERN - Pattern to search for
  FILE-TYPE - Optional file type filter (e.g., \\='js\\=', \\='py\\=', \\='rb\\=')
  
  Provides powerful text search capabilities with regex support and file type constraints."
  (progn
    (condition-case err
        (let* ((type-mapping '(("rb" . "ruby") ("py" . "py") ("js" . "js") 
                               ("ts" . "ts") ("go" . "go") ("rs" . "rust")
                               ("erb" . "ruby") ("haml" . "ruby") ("slim" . "ruby")
                               ("jsx" . "js") ("vue" . "js") ("svelte" . "js")
                               ("tsx" . "ts") ("html" . "html") ("htm" . "html")
                               ("tmpl" . "go") ("gotmpl" . "go")))
               (rg-type (when file-type 
                          (or (cdr (assoc file-type type-mapping)) file-type)))
               (command (if rg-type
                            (format "rg --type %s '%s'" rg-type pattern)
                          (format "rg '%s'" pattern)))
               (results (shell-command-to-string command)))
          (if (> (length results) 0)
              (format "Search results for '%s'%s:\n%s" 
                      pattern 
                      (if file-type (format " (type: %s)" file-type) "")
                      results)
            (format "No matches found for '%s'%s" 
                    pattern 
                    (if file-type (format " in %s files" file-type) ""))))
      (error
       (format "Error searching for '%s': %s"
               pattern (error-message-string err))))))

(define-mcp-tool consult-grep (pattern)
  "Interactive grep search using consult, returning top matches with context.
  
  PATTERN - Pattern to search for
  
  Ideal for exploring search results with live preview."
  (progn
    (condition-case err
        (let* ((default-directory (or (projectile-project-root) default-directory))
               (cmd (format "rg -n --max-count 20 '%s' ." pattern))
               (results (shell-command-to-string cmd)))
          (if (string-empty-p (string-trim results))
              (format "No matches found for pattern '%s'" pattern)
            (format "Grep results for '%s':\n%s" pattern results)))
      (error
       (format "Error running consult grep for '%s': %s"
               pattern (error-message-string err))))))

(define-mcp-tool projectile-list-projects ()
  "List all known projectile projects.
  
  Essential for understanding available projects before switching."
  (progn
    (condition-case err
        (if (not (bound-and-true-p projectile-mode))
            "Projectile mode is not active"
          (if projectile-known-projects
              (mapcar (lambda (project)
                        (format "%s" project))
                      projectile-known-projects)
            "No known projects found"))
      (error
       (format "Error listing projects: %s" (error-message-string err))))))

(define-mcp-tool projectile-switch-project (project-path)
  "Switch to a specific project by path.
  
  PROJECT-PATH - Path to the project directory
  
  Use projectile_list_projects first to see available options."
  (progn
    (condition-case err
        (if (not (bound-and-true-p projectile-mode))
            "Projectile mode is not active"
          (if (file-directory-p project-path)
              (progn
                ;; Set default directory without interactive prompts
                (setq default-directory (file-name-as-directory project-path))
                ;; Add to known projects if not already there
                (projectile-add-known-project project-path)
                ;; Switch context by opening a file in the project
                (let ((project-files (projectile-project-files project-path)))
                  (when project-files
                    (find-file-noselect (expand-file-name (car project-files) project-path))))
                (format "Switched to project: %s" project-path))
            (format "Project directory does not exist: %s" project-path)))
      (error
       (format "Error switching to project %s: %s" 
               project-path (error-message-string err))))))

(define-mcp-tool projectile-current-project ()
  "Get detailed information about the current project including type and file count."
  (progn
    (condition-case err
        (if (not (bound-and-true-p projectile-mode))
            "Projectile mode is not active"
          (let ((project-root (projectile-project-root)))
            (if project-root
                (format "Current project: %s\nType: %s\nFiles: %d"
                        project-root
                        (projectile-project-type project-root)
                        (length (projectile-current-project-files)))
              "Not in a project")))
      (error
       (format "Error getting current project info: %s" (error-message-string err))))))

(define-mcp-tool list-open-buffers ()
  "List all open buffers with their associated file paths.
  
  Excludes special system buffers."
  (progn
    (condition-case err
        (let ((buffers (buffer-list))
              (results '()))
          (dolist (buffer buffers)
            (let ((name (buffer-name buffer))
                  (file (buffer-file-name buffer)))
              (unless (string-match-p "^\\*" name) ; Skip special buffers
                (if file
                    (push (format "%s -> %s" name file) results)
                  (push (format "%s (no file)" name) results)))))
          (if results
              (nreverse results)
            "No regular buffers open"))
      (error
       (format "Error listing buffers: %s" (error-message-string err))))))

(define-mcp-tool recent-files ()
  "List recently opened files (up to 20).
  
  Requires recentf-mode to be active."
  (progn
    (condition-case err
        (progn
          ;; Enable recentf-mode if not already active
          (unless (bound-and-true-p recentf-mode)
            (recentf-mode 1))
          (if recentf-list
              (cl-subseq recentf-list 0 (min 20 (length recentf-list)))
            "No recent files found (recentf just enabled)"))
      (error
       (format "Error getting recent files: %s" (error-message-string err))))))

(provide 'claude-code-ide-project-tools)
;;; claude-code-ide-project-tools.el ends here