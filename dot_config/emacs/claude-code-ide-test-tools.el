;;; claude-code-ide-test-tools.el --- Testing framework integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: ai, claude, mcp, testing, projectile

;;; Commentary:

;; Testing framework integration for Claude Code IDE.
;; Uses Projectile's test infrastructure for monorepo-aware test execution.

;;; Code:

(require 'claude-code-ide-mcp-macros)

;; Testing and compilation declarations
(declare-function projectile-test-project "projectile")
(declare-function projectile-test-command "projectile")
(declare-function projectile-project-type "projectile")
(declare-function projectile-find-test-file "projectile")
(declare-function projectile-test-suffix "projectile")
(declare-function projectile-test-directory "projectile")
(declare-function projectile-project-root "projectile")
(declare-function compile "compile")

(defun claude-code-custom-get-workspace-root (&optional file-path)
  "Get the appropriate workspace root for FILE-PATH, preferring LSP over projectile.
If FILE-PATH is nil, uses the current buffer's workspace."
  (if file-path
      ;; Get workspace for specific file
      (let ((target-buffer (or (find-buffer-visiting file-path)
                               (find-file-noselect file-path))))
        (with-current-buffer target-buffer
          (or (and (bound-and-true-p lsp-mode)
                   (condition-case nil (lsp-workspace-root) (error nil)))
              (projectile-project-root)
              default-directory)))
    ;; Get workspace for current buffer
    (or (and (bound-and-true-p lsp-mode)
             (condition-case nil (lsp-workspace-root) (error nil)))
        (projectile-project-root)
        default-directory)))

(define-mcp-tool detect-test-framework ()
  "Detect test configuration for the current project using Projectile.
  
  Shows configured test commands and provides setup guidance for monorepos.
  
  For monorepos, configure via .dir-locals.el for custom commands per subproject:
    ((nil . ((projectile-project-test-cmd . \"bundle exec rspec\"))))"
  (progn
    (condition-case err
        (if (bound-and-true-p projectile-mode)
            (let* ((project-root (projectile-project-root))
                   (project-type (projectile-project-type))
                   (test-cmd (projectile-test-command project-type))
                   (test-suffix (projectile-test-suffix project-type))
                   (test-dir (projectile-test-directory project-type)))
              (format "Project: %s
Type: %s
Test command: %s
Test suffix: %s
Test directory: %s

Note: For monorepos, configure via .dir-locals.el for custom commands per subproject:
  ((nil . ((projectile-project-test-cmd . \"bundle exec rspec\"))))"
                      project-root
                      (or project-type "unknown")
                      (or test-cmd "not configured")
                      (or test-suffix "not configured")
                      (or test-dir "not configured")))
          "Projectile mode is not active")
      (error
       (format "Error detecting test framework: %s" (error-message-string err))))))

(define-mcp-tool run-file-tests (file-path)
  "Run tests for a specific file using Projectile's test infrastructure.
  
  FILE-PATH - Path to the test file to run
  
  This attempts to find and run the corresponding test file.
  For best results, ensure your project has proper test suffix configuration."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (if (bound-and-true-p projectile-mode)
                  (let ((test-file (projectile-find-test-file file-path))
                        (project-root (projectile-project-root))
                        (project-type (projectile-project-type)))
                    (if test-file
                        (let ((test-cmd (projectile-test-command project-type)))
                          (if test-cmd
                              (progn
                                (compile (format "%s %s" test-cmd (file-relative-name file-path project-root)))
                                (format "Running test for %s using: %s" file-path test-cmd))
                            (format "Test file found (%s) but no test command configured for project type: %s
Please configure via .dir-locals.el:
  ((nil . ((projectile-project-test-cmd . \"bundle exec rspec\"))))" 
                                    test-file project-type)))
                      (format "No test file found for %s. 
Projectile looks for files with test suffix: %s
Configure test suffix in .dir-locals.el if needed:
  ((nil . ((projectile-test-suffix . \"_test\"))))" 
                              file-path 
                              (or (projectile-test-suffix project-type) "not configured"))))
                "Projectile mode is not active")))
        (error
         (format "Error running tests for %s: %s"
                 file-path (error-message-string err)))))))

(define-mcp-tool run-project-tests ()
  "Run all tests for the current project using Projectile's configured test command.
  
  Supports .dir-locals.el configuration for monorepos.
  
  For monorepos, ensure each subproject has proper .dir-locals.el configuration:
    ((nil . ((projectile-project-test-cmd . \"bundle exec rspec\"))))
  If this fails, ask the user to configure their project's test command."
  (progn
    (condition-case err
        (if (bound-and-true-p projectile-mode)
            (let* ((project-root (projectile-project-root))
                   (project-type (projectile-project-type))
                   (test-cmd (projectile-test-command project-type))
                   (default-directory project-root))
              (if test-cmd
                  (progn
                    (compile test-cmd)
                    (format "Running tests with: %s in %s" test-cmd project-root))
                (format "No test command configured for project at %s. 
Please configure via .dir-locals.el:
  ((nil . ((projectile-project-test-cmd . \"your-test-command\"))))
Or register the project type with Projectile." project-root)))
          "Projectile mode is not active")
      (error
       (format "Error running project tests: %s" (error-message-string err))))))

(provide 'claude-code-ide-test-tools)
;;; claude-code-ide-test-tools.el ends here