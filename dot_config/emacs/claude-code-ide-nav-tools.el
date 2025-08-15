;;; claude-code-ide-nav-tools.el --- Navigation and cursor management tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: ai, claude, mcp, navigation, cursor

;;; Commentary:

;; Navigation and cursor management tools for Claude Code IDE.
;; Provides precise cursor positioning, bookmarking, and symbol navigation.

;;; Code:

(require 'claude-code-ide-mcp-macros)

;; Navigation and imenu declarations
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu "imenu")
(defvar imenu--index-alist)

(defvar claude-code-custom-saved-positions nil
  "Alist of saved cursor positions per file.")

(defun claude-code-find-imenu-item (symbol-name imenu-alist)
  "Search for SYMBOL-NAME in IMENU-ALIST with multiple strategies.
Returns the item if found, nil otherwise.
Tries exact match first, then partial match, then substring match."
  (or 
   ;; Strategy 1: Exact match
   (claude-code-find-imenu-item-exact symbol-name imenu-alist)
   ;; Strategy 2: Partial match (symbol contains the name)
   (claude-code-find-imenu-item-partial symbol-name imenu-alist)
   ;; Strategy 3: Look for qualified names ending with symbol-name
   (claude-code-find-imenu-item-suffix symbol-name imenu-alist)))

(defun claude-code-find-imenu-item-exact (symbol-name imenu-alist)
  "Find exact match for SYMBOL-NAME in IMENU-ALIST."
  (catch 'found
    (dolist (item imenu-alist)
      (cond
       ;; Direct match at top level
       ((and (consp item) (string= (car item) symbol-name))
        (throw 'found item))
       ;; Search in subcategories
       ((and (consp item) (listp (cdr item)))
        (let ((subitem (claude-code-find-imenu-item-exact symbol-name (cdr item))))
          (when subitem
            (throw 'found subitem))))))
    nil))

(defun claude-code-find-imenu-item-partial (symbol-name imenu-alist)
  "Find partial match for SYMBOL-NAME in IMENU-ALIST."
  (catch 'found
    (dolist (item imenu-alist)
      (cond
       ;; Partial match at top level (item name contains symbol-name)
       ((and (consp item) (string-match-p (regexp-quote symbol-name) (car item)))
        (throw 'found item))
       ;; Search in subcategories
       ((and (consp item) (listp (cdr item)))
        (let ((subitem (claude-code-find-imenu-item-partial symbol-name (cdr item))))
          (when subitem
            (throw 'found subitem))))))
    nil))

(defun claude-code-find-imenu-item-suffix (symbol-name imenu-alist)
  "Find qualified name ending with SYMBOL-NAME in IMENU-ALIST."
  (catch 'found
    (dolist (item imenu-alist)
      (cond
       ;; Suffix match at top level (item name ends with #symbol-name or similar)
       ((and (consp item) 
             (string-match-p (concat "[#\\.]" (regexp-quote symbol-name) "$") (car item)))
        (throw 'found item))
       ;; Search in subcategories
       ((and (consp item) (listp (cdr item)))
        (let ((subitem (claude-code-find-imenu-item-suffix symbol-name (cdr item))))
          (when subitem
            (throw 'found subitem))))))
    nil))

(define-mcp-tool goto-line-column (file-path line column)
  "Move cursor to a specific line and column in a file.
  
  FILE-PATH - Path to the file
  LINE - Line number (1-based)
  COLUMN - Column number (0-based, optional)
  
  Essential for precise navigation before using at-point tools like LSP functions,
  TreeSitter analysis, or code manipulation. The cursor position will persist."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (goto-char (point-min))
              (forward-line (1- line))
              (move-to-column (or column 0))
              (format "Moved to %s:%d:%d (point: %d)" 
                      file-path line (or column 0) (point))))
        (error
         (format "Error moving to %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(define-mcp-tool get-cursor-position (file-path)
  "Get the current cursor position in a file.
  
  FILE-PATH - Path to the file to check
  
  Returns line, column, and point information. Use this to understand where 
  you are before navigation or to verify position after goto_line_column."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (let ((line (line-number-at-pos))
                    (column (current-column))
                    (point (point)))
                (format "Position in %s: line %d, column %d (point: %d)"
                        file-path line column point))))
        (error
         (format "Error getting cursor position in %s: %s"
                 file-path (error-message-string err)))))))

(define-mcp-tool save-cursor-position (file-path name)
  "Save the current cursor position with a name for later restoration.
  
  FILE-PATH - Path to the file
  NAME - Name to save the position under (e.g., \\='start_point\\=', \\='error_location\\=')
  
  Use this to create a bookmark before navigating elsewhere, so you can return 
  later with restore_cursor_position. Helpful workflow: save position → 
  navigate → work → restore position."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (let ((position (point))
                    (line (line-number-at-pos))
                    (column (current-column)))
                (setq claude-code-custom-saved-positions
                      (cons (list name file-path position line column)
                            (assq-delete-all name claude-code-custom-saved-positions)))
                (format "Saved position '%s' in %s at line %d, column %d"
                        name file-path line column))))
        (error
         (format "Error saving cursor position in %s: %s"
                 file-path (error-message-string err)))))))

(define-mcp-tool restore-cursor-position (name)
  "Restore a previously saved cursor position by name.
  
  NAME - Name of the saved position to restore
  
  The cursor position will persist after restoration."
  (progn
    (condition-case err
        (let ((saved-pos (assoc name claude-code-custom-saved-positions)))
          (if saved-pos
              (let ((file-path (nth 1 saved-pos))
                    (position (nth 2 saved-pos))
                    (line (nth 3 saved-pos))
                    (column (nth 4 saved-pos)))
                (let ((target-buffer (or (find-buffer-visiting file-path)
                                         (find-file-noselect file-path))))
                  (with-current-buffer target-buffer
                    (goto-char position)
                    (format "Restored position '%s' in %s to line %d, column %d"
                            name file-path line column))))
            (format "No saved position found with name '%s'" name)))
      (error
       (format "Error restoring cursor position '%s': %s"
               name (error-message-string err))))))

(define-mcp-tool goto-symbol (file-path symbol-name)
  "Jump to a specific symbol in a file using imenu.
  
  FILE-PATH - Path to the file
  SYMBOL-NAME - Name of the symbol to jump to
  
  Use imenu_list_all_symbols first to see available symbols. The cursor position will persist."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (imenu--make-index-alist)
              (if imenu--index-alist
                  (let ((item (claude-code-find-imenu-item symbol-name imenu--index-alist)))
                    (if item
                        (progn
                          (imenu item)
                          (format "Jumped to symbol '%s' in %s at line %d"
                                  symbol-name file-path (line-number-at-pos)))
                      (format "Symbol '%s' not found in %s" symbol-name file-path)))
                (format "No imenu support for %s" file-path))))
        (error
         (format "Error jumping to symbol '%s' in %s: %s"
                 symbol-name file-path (error-message-string err)))))))

(provide 'claude-code-ide-nav-tools)
;;; claude-code-ide-nav-tools.el ends here