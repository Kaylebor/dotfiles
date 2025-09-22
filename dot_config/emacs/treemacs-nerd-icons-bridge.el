;;; treemacs-nerd-icons-bridge.el --- Extend Treemacs nerd-icons coverage -*- lexical-binding: t; -*-

(require 'treemacs)
(require 'treemacs-nerd-icons nil t)
(require 'nerd-icons)

(defvar my/treemacs-nerd-icons--regex-exported nil
  "Internal guard to ensure regex icons are mirrored once per session.")

(defun my/treemacs-nerd-icons--literal-pattern (pattern)
  "Return a literal filename when PATTERN is a simple anchored regex."
  (when (and (string-prefix-p "^" pattern)
             (string-suffix-p "$" pattern))
    (let ((body (substring pattern 1 (1- (length pattern)))))
      (unless (string-match-p "[\\[(){}*+?|]" body)
        (replace-regexp-in-string "\\\\." "." body t t)))))

(defun my/treemacs-nerd-icons-sync-regex ()
  "Mirror nerd-icons regex icons into the active Treemacs theme."
  (unless my/treemacs-nerd-icons--regex-exported
    (setq my/treemacs-nerd-icons--regex-exported t)
    (when (boundp 'nerd-icons-regexp-icon-alist)
      (dolist (entry nerd-icons-regexp-icon-alist)
        (pcase-let ((`(,pattern ,fn ,icon-name . ,props) entry))
          (let ((literal (my/treemacs-nerd-icons--literal-pattern pattern)))
            (when (and literal (functionp fn))
              (let* ((args (append (list icon-name) props))
                     (args (if (plist-member props :height)
                               args
                             (append args '(:height 1.0))))
                     (args (if (plist-member props :v-adjust)
                               args
                             (append args '(:v-adjust -0.05))))
                     (glyph (apply fn args))
                     (icon-str (format " %s%s%s"
                                       treemacs-nerd-icons-tab
                                       glyph
                                       treemacs-nerd-icons-tab)))
                (treemacs-define-custom-icon icon-str literal))))))))

(defun my/treemacs-nerd-icons-apply-overrides ()
  "Tweak specific Treemacs icons for better consistency."
  ;; Prefer a key glyph for LICENSE files.
  (dolist (filename '("LICENSE" "LICENSE.md"))
    (treemacs-define-custom-icon
     (format " %s%s%s"
             treemacs-nerd-icons-tab
             (nerd-icons-faicon "nf-fa-key"
                                :face 'treemacs-nerd-icons-file-face
                                :height 1.0
                                :v-adjust -0.05)
             treemacs-nerd-icons-tab)
     filename))
  ;; Align special src folder icons with the base folder glyphs.
  (let* ((gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
         (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
         (tab treemacs-nerd-icons-tab)
         (open-icon (format "%s%s%s%s"
                            (nerd-icons-octicon "nf-oct-chevron_down"
                                                :face 'treemacs-nerd-icons-file-face
                                                :height 1.0
                                                :v-adjust -0.05)
                            tab
                            (nerd-icons-faicon "nf-fa-folder_open"
                                               :face 'treemacs-nerd-icons-file-face
                                               :height 1.0
                                               :v-adjust -0.05)
                            tab))
         (closed-icon (format "%s%s%s%s"
                               (nerd-icons-octicon "nf-oct-chevron_right"
                                                   :face 'treemacs-nerd-icons-file-face
                                                   :height 1.0
                                                   :v-adjust -0.05)
                               tab
                               (nerd-icons-faicon "nf-fa-folder"
                                                  :face 'treemacs-nerd-icons-file-face
                                                  :height 1.0
                                                  :v-adjust -0.05)
                               tab))
         (open-tui (treemacs-get-icon-value "src-open" t))
         (closed-tui (treemacs-get-icon-value "src-closed" t)))
    (ht-set! gui-icons "src-open" open-icon)
    (ht-set! gui-icons "src-closed" closed-icon)
    (ht-set! tui-icons "src-open" (or open-tui (treemacs-get-icon-value "dir-open" t)))
    (ht-set! tui-icons "src-closed" (or closed-tui (treemacs-get-icon-value "dir-closed" t)))))

  ;; Provide glyph-based icons for LSP symbol kinds. lsp-mode reuses these
  ;; through `lsp-treemacs-symbol-icon' for breadcrumbs and headerline UI.
  (let* ((tab treemacs-nerd-icons-tab)
         (kind-map
          '((document      nerd-icons-codicon "nf-cod-file"               nerd-icons-blue)
            (namespace     nerd-icons-codicon "nf-cod-package"            nerd-icons-lblue)
            (class         nerd-icons-codicon "nf-cod-symbol_class"       nerd-icons-purple)
            (interface     nerd-icons-codicon "nf-cod-symbol_interface"   nerd-icons-cyan)
            (method        nerd-icons-codicon "nf-cod-symbol_method"      nerd-icons-orange)
            (property      nerd-icons-codicon "nf-cod-symbol_property"    nerd-icons-lyellow)
            (field         nerd-icons-codicon "nf-cod-symbol_field"       nerd-icons-lcyan)
            (enumerator    nerd-icons-codicon "nf-cod-symbol_enum_member" nerd-icons-dorange)
            (localvariable nerd-icons-codicon "nf-cod-symbol_variable"    nerd-icons-lgreen)
            (constant      nerd-icons-codicon "nf-cod-symbol_constant"    nerd-icons-dpurple)
            (string        nerd-icons-codicon "nf-cod-symbol_string"      nerd-icons-lyellow)
            (numeric       nerd-icons-codicon "nf-cod-symbol_numeric"     nerd-icons-blue)
            (boolean-data  nerd-icons-codicon "nf-cod-symbol_boolean"     nerd-icons-green)
            (indexer       nerd-icons-codicon "nf-cod-symbol_key"         nerd-icons-lorange)
            (enumitem      nerd-icons-codicon "nf-cod-symbol_enum"        nerd-icons-dorange)
            (structure     nerd-icons-codicon "nf-cod-symbol_structure"   nerd-icons-lblue)
            (event         nerd-icons-codicon "nf-cod-symbol_event"       nerd-icons-yellow)
            (operator      nerd-icons-codicon "nf-cod-symbol_operator"    nerd-icons-red)
            (template      nerd-icons-codicon "nf-cod-symbol_snippet"     nerd-icons-pink)
            (misc          nerd-icons-codicon "nf-cod-symbol_misc"        nerd-icons-dsilver))))
    (dolist (entry kind-map)
      (pcase-let ((`(,key ,fn ,icon-name ,face) entry))
        (let* ((string-key (if (symbolp key) (symbol-name key) key))
               (symbol-key (if (symbolp key) key (intern key)))
               (icon (format " %s%s%s"
                             tab
                             (funcall fn icon-name
                                      :face face
                                      :height 1.0
                                      :v-adjust -0.05)
                             tab)))
          (treemacs-define-custom-icon icon string-key)
          (when (symbolp key)
            (let ((gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
                  (tui-icons (treemacs-theme->tui-icons treemacs--current-theme)))
              (ht-set! gui-icons symbol-key (ht-get gui-icons string-key))
              (ht-set! tui-icons symbol-key (ht-get tui-icons string-key)))))))))

(defun my/treemacs-nerd-icons-bridge-init ()
  "Apply regex mirroring and overrides to the current Treemacs theme."
  (when (featurep 'treemacs)
    (my/treemacs-nerd-icons-sync-regex)
    (my/treemacs-nerd-icons-apply-overrides)))

(my/treemacs-nerd-icons-bridge-init)

(provide 'treemacs-nerd-icons-bridge)

;;; treemacs-nerd-icons-bridge.el ends here
