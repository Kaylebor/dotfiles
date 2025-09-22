;;; lsp-nerd-icons-bridge.el --- Provide all-the-icons shims via nerd-icons -*- lexical-binding: t; -*-

(require 'nerd-icons)
(require 'cl-lib)

(defvaralias 'all-the-icons-scale-factor 'nerd-icons-scale-factor)
(defvaralias 'all-the-icons-default-adjust 'nerd-icons-default-adjust)
(defvaralias 'all-the-icons-color-icons 'nerd-icons-color-icons)

(defun lsp-nerd-icons--normalize-args (plist)
  "Ensure PLIST carries :height and :v-adjust defaults."
  (let ((plist (copy-sequence plist)))
    (unless (plist-member plist :height)
      (setq plist (plist-put plist :height all-the-icons-scale-factor)))
    (unless (plist-member plist :v-adjust)
      (setq plist (plist-put plist :v-adjust all-the-icons-default-adjust)))
    plist))

(defun lsp-nerd-icons--canonical-name (prefix icon-name)
  "Turn ICON-NAME into the Nerd Font name using PREFIX."
  (let* ((name (if (symbolp icon-name)
                   (symbol-name icon-name)
                 (format "%s" icon-name)))
         (name (downcase name))
         (name (replace-regexp-in-string " " "_" name))
         (name (replace-regexp-in-string "-" "_" name)))
    (if (string-prefix-p "nf-" name)
        name
      (concat prefix name))))

(defun lsp-nerd-icons--dispatch (fn prefix icon-name args)
  "Invoke FN with ICON-NAME after translating to Nerd Font naming."
  (let* ((plist (lsp-nerd-icons--normalize-args args))
         (full-name (lsp-nerd-icons--canonical-name prefix icon-name)))
    (condition-case _
        (apply fn full-name plist)
      (error
       ;; Fall back to simple text if icon lookup fails.
       (propertize (format "%s" icon-name)
                   'face (plist-get plist :face))))))

(defmacro lsp-nerd-icons-define-shim (name fn prefix)
  "Define all-the-icons shim NAME delegating to nerd-icons FN."
  `(unless (fboundp ',name)
     (defun ,name (icon-name &rest args)
       (lsp-nerd-icons--dispatch #',fn ,prefix icon-name args))))

(lsp-nerd-icons-define-shim all-the-icons-material  nerd-icons-mdicon   "nf-md-")
(lsp-nerd-icons-define-shim all-the-icons-octicon   nerd-icons-octicon  "nf-oct-")
(lsp-nerd-icons-define-shim all-the-icons-faicon    nerd-icons-faicon   "nf-fa-")
(lsp-nerd-icons-define-shim all-the-icons-codicon   nerd-icons-codicon  "nf-cod-")
(lsp-nerd-icons-define-shim all-the-icons-devicon   nerd-icons-devicon  "nf-dev-")
(lsp-nerd-icons-define-shim all-the-icons-wicon     nerd-icons-wicon    "nf-weather-")
(lsp-nerd-icons-define-shim all-the-icons-sucicon   nerd-icons-sucicon  "nf-seti-")
(lsp-nerd-icons-define-shim all-the-icons-flicon    nerd-icons-flicon   "nf-linux-")
(lsp-nerd-icons-define-shim all-the-icons-pomicon   nerd-icons-pomicon  "nf-pom-")
(lsp-nerd-icons-define-shim all-the-icons-powerline nerd-icons-powerline "nf-pl-")
(lsp-nerd-icons-define-shim all-the-icons-ipsicon   nerd-icons-ipsicon  "nf-iec-" )

;; Families – satisfy callers that expect them.
(unless (fboundp 'all-the-icons-material-family)
  (defun all-the-icons-material-family () nerd-icons-font-family))

(unless (fboundp 'all-the-icons-octicon-family)
  (defun all-the-icons-octicon-family () nerd-icons-font-family))

(provide 'lsp-nerd-icons-bridge)
;;; lsp-nerd-icons-bridge.el ends here
