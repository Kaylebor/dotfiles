;; -*- lexical-binding: t; -*-
;; Composite modes for chezmoi template files (.tmpl)
;; These combine go-template grammar with the target language grammar

(defun chezmoi-template-find-template-ranges ()
  "Find ranges in buffer that ARE template syntax ({{...}} or {%...%})."
  (let ((ranges '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "{{-?\\|{%-?" nil t)
        (let ((template-start (match-beginning 0)))
          (when (re-search-forward "-?}}\\|-?%}" nil t)
            (push (cons template-start (point)) ranges)))))
    (nreverse ranges)))

(defun chezmoi-template-find-non-template-ranges ()
  "Find ranges in buffer that are NOT template syntax."
  (let ((template-ranges (chezmoi-template-find-template-ranges))
        (ranges '())
        (pos 1))
    (if (null template-ranges)
        ;; No templates, entire buffer is non-template
        (list (cons 1 (point-max)))
      ;; Build non-template ranges between template ranges
      (dolist (tmpl-range template-ranges)
        (let ((tmpl-start (car tmpl-range)))
          (when (< pos tmpl-start)
            (push (cons pos tmpl-start) ranges))
          (setq pos (cdr tmpl-range))))
      ;; Add remaining range after last template
      (when (< pos (point-max))
        (push (cons pos (point-max)) ranges))
      (nreverse ranges))))

(defun chezmoi-template-setup-gotmpl-font-lock (embed-lang)
  "Set up font-lock rules for gotmpl template syntax.
EMBED-LANG is the embedded language for context."
  ;; Get existing font-lock settings from the base mode (if any)
  (let ((base-settings (or treesit-font-lock-settings '())))
    ;; Add gotmpl-specific font-lock rules
    (setq-local treesit-font-lock-settings
                (append
                 ;; gotmpl font-lock rules
                 (treesit-font-lock-rules
                  :language 'gotmpl
                  :feature 'delimiter
                  '((["{{" "}}" "{{-" "-}}" "{%" "%}" "{%-" "-%}"] @font-lock-preprocessor-face)
                    ("|" @font-lock-operator-face))
                  
                  :language 'gotmpl
                  :feature 'keyword
                  '((["else" "if" "range" "with" "end" "template" "define" "block"] @font-lock-keyword-face))
                  
                  :language 'gotmpl
                  :feature 'variable
                  '((variable) @font-lock-variable-name-face
                    (field) @font-lock-property-name-face
                    (field_identifier) @font-lock-property-name-face)
                  
                  :language 'gotmpl
                  :feature 'function
                  '((function_call) @font-lock-function-call-face
                    (method_call) @font-lock-function-call-face)
                  
                  :language 'gotmpl
                  :feature 'operator
                  '((["|" ":=" "=" "!=" "<" ">" "<=" ">=" "and" "or" "not"] @font-lock-operator-face))
                  
                  :language 'gotmpl
                  :feature 'literal
                  '((interpreted_string_literal) @font-lock-string-face
                    (raw_string_literal) @font-lock-string-face
                    (rune_literal) @font-lock-string-face
                    (escape_sequence) @font-lock-escape-face
                    (int_literal) @font-lock-number-face
                    (float_literal) @font-lock-number-face
                    (imaginary_literal) @font-lock-number-face
                    (true) @font-lock-constant-face
                    (false) @font-lock-constant-face
                    (nil) @font-lock-constant-face)
                  
                  :language 'gotmpl
                  :feature 'comment
                  '((comment) @font-lock-comment-face)
                  
                  :language 'gotmpl
                  :feature 'punctuation
                  '((["." "," ":" ";" "(" ")" "[" "]" "{" "}"] @font-lock-punctuation-face)))
                 
                 ;; Keep existing base settings
                 base-settings))
    
    ;; Enable all gotmpl features
    (setq-local treesit-font-lock-feature-list
                (let ((base-features (or treesit-font-lock-feature-list
                                         '((comment)
                                           (keyword string)
                                           (function variable property)
                                           (bracket delimiter operator)))))
                  ;; Ensure gotmpl features are included
                  (mapcar (lambda (level)
                            (seq-uniq (append level
                                            '(delimiter keyword variable function
                                              operator literal comment punctuation))))
                          base-features)))))

(defun chezmoi-template-setup-range-update (embed-lang)
  "Set up automatic range updates when buffer changes.
EMBED-LANG is the embedded language."
  ;; Add a buffer change hook to update ranges
  (add-hook 'after-change-functions
            (lambda (beg end _len)
              ;; Update ranges for both parsers
              (let ((template-ranges (chezmoi-template-find-template-ranges))
                    (non-template-ranges (chezmoi-template-find-non-template-ranges)))
                ;; Update each parser's ranges
                (dolist (parser (treesit-parser-list))
                  (let ((lang (treesit-parser-language parser)))
                    (cond
                     ((eq lang 'gotmpl)
                      (treesit-parser-set-included-ranges parser template-ranges))
                     ((eq lang embed-lang)
                      (treesit-parser-set-included-ranges parser non-template-ranges)))))))
            nil t))

(defun chezmoi-template-setup (base-mode embed-lang)
  "Set up a chezmoi template mode.
BASE-MODE is the major mode for the embedded content.
EMBED-LANG is the tree-sitter language for the embedded content (e.g., 'yaml, 'bash)."
  
  ;; Activate the base mode
  (funcall base-mode)
  
  ;; Set up dual-language parsing if both grammars are available
  (cond
   ((and embed-lang (treesit-ready-p embed-lang) (treesit-ready-p 'gotmpl))
    ;; Remove any parsers the base mode created
    (dolist (parser (treesit-parser-list))
      (treesit-parser-delete parser))
    
    ;; Find template and non-template ranges
    (let ((template-ranges (chezmoi-template-find-template-ranges))
          (non-template-ranges (chezmoi-template-find-non-template-ranges)))
      
      ;; Create the embedded language parser as primary (for font-lock)
      (let ((embed-parser (treesit-parser-create embed-lang)))
        (treesit-parser-set-included-ranges embed-parser non-template-ranges)
        (setq-local treesit-primary-parser embed-parser))
      
      ;; Create the gotmpl parser for template regions only
      (let ((gotmpl-parser (treesit-parser-create 'gotmpl)))
        (treesit-parser-set-included-ranges gotmpl-parser template-ranges))
      
      ;; Tell Emacs which language is at a given point
      (setq-local treesit-language-at-point-function
                  (lambda (pos)
                    ;; Check if pos is within any template range
                    (let ((in-template nil))
                      (dolist (range template-ranges)
                        (when (and (>= pos (car range))
                                  (< pos (cdr range)))
                          (setq in-template t)))
                      (if in-template 'gotmpl embed-lang))))
      
      ;; Set up font-lock for gotmpl
      (chezmoi-template-setup-gotmpl-font-lock embed-lang)
      
      ;; Set up range update on buffer change
      (chezmoi-template-setup-range-update embed-lang))
    
    ;; Re-setup the major mode features
    (treesit-major-mode-setup))
   
   ;; Fallback: only gotmpl available
   ((treesit-ready-p 'gotmpl)
    (message "Note: Install tree-sitter grammar for %s for full syntax highlighting" embed-lang))
   
   ;; Fallback: only embedded language available
   ((and embed-lang (treesit-ready-p embed-lang))
    (message "Note: Install gotmpl tree-sitter grammar for template syntax highlighting"))))

(defun chezmoi-template-debug-ranges ()
  "Debug function to inspect parser ranges in current buffer."
  (interactive)
  (let ((parsers (treesit-parser-list)))
    (message "=== Parsers in buffer: %s ===" (length parsers))
    (dolist (parser parsers)
      (let* ((lang (treesit-parser-language parser))
             (ranges (treesit-parser-included-ranges parser)))
        (message "Parser %s ranges: %s" lang ranges)))
    
    ;; Explore the gotmpl tree structure
    (when (treesit-ready-p 'gotmpl)
      (let ((root (treesit-buffer-root-node 'gotmpl)))
        (message "=== gotmpl tree structure ===")
        (chezmoi-template-explore-node root 0)))))

(defun chezmoi-template-explore-node (node level)
  "Recursively explore tree-sitter node structure."
  (when (and node (< level 3))  ; Limit depth to avoid spam
    (let ((type (treesit-node-type node))
          (start (treesit-node-start node))
          (end (treesit-node-end node))
          (text (treesit-node-text node)))
      (message "%s%s [%s-%s]: %s"
               (make-string (* level 2) ?\s)
               type start end
               (if (< (length text) 50)
                   text
                 (concat (substring text 0 47) "..."))))
    
    ;; Explore children
    (let ((child (treesit-node-child node 0))
          (i 0))
      (while (and child (< i 10))  ; Limit to first 10 children
        (chezmoi-template-explore-node child (1+ level))
        (setq i (1+ i))
        (setq child (treesit-node-next-sibling child))))))

(defun chezmoi-template-mode ()
  "Automatically set up the right composite mode for a .tmpl file."
  (interactive)
  (let* ((filename (buffer-file-name))
         (base-name (file-name-sans-extension filename)))
    (cond
      ;; YAML files
      ((string-match "\\.ya?ml\\'" base-name)
       (chezmoi-template-setup 'yaml-ts-mode 'yaml))
      
      ;; Fish shell files
      ((string-match "\\.fish\\'" base-name)
       (chezmoi-template-setup 'bash-ts-mode 'bash))
      
      ;; Emacs Lisp files
      ((string-match "\\.el\\'" base-name)
       (chezmoi-template-setup 'emacs-lisp-mode 'elisp))
      
      ;; TOML files
      ((string-match "\\.toml\\'" base-name)
       (chezmoi-template-setup 'toml-ts-mode 'toml))
      
      ;; JSON files
      ((string-match "\\.json\\'" base-name)
       (chezmoi-template-setup 'json-ts-mode 'json))
      
      ;; Bash/shell files
      ((string-match "\\.\\(sh\\|bash\\)\\'" base-name)
       (chezmoi-template-setup 'bash-ts-mode 'bash))
      
      ;; Zsh files
      ((string-match "\\.zsh\\'" base-name)
       (chezmoi-template-setup 'bash-ts-mode 'bash))
      
      ;; Markdown files
      ((string-match "\\.md\\'" base-name)
       (if (fboundp 'markdown-ts-mode)
           (chezmoi-template-setup 'markdown-ts-mode 'markdown)
         (chezmoi-template-setup 'markdown-mode nil)))
      
      ;; Nushell files
      ((string-match "\\.nu\\'" base-name)
       (chezmoi-template-setup 'prog-mode nil))
      
      ;; INI/conf files
      ((string-match "\\.\\(ini\\|conf\\)\\'" base-name)
       (chezmoi-template-setup 'conf-mode nil))
      
      ;; Plain text files
      ((string-match "\\.txt\\'" base-name)
       (chezmoi-template-setup 'text-mode nil))
      
      ;; Shell-related files without extensions (dot_bashrc.tmpl, dot_zshrc.tmpl, etc.)
      ((string-match "\\(bashrc\\|bash_profile\\|profile\\|zshrc\\|zprofile\\)" base-name)
       (chezmoi-template-setup 'bash-ts-mode 'bash))
      
      ;; Git config files
      ((string-match "gitconfig" base-name)
       (chezmoi-template-setup 'conf-mode nil))
      
      ;; Default: just use fundamental mode with gotmpl
      (t
       (chezmoi-template-setup 'fundamental-mode nil)))))

;; Associate .tmpl files with our dynamic mode
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . chezmoi-template-mode))

(provide 'chezmoi-templates)