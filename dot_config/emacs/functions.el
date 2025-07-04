;; -*- lexical-binding: t; -*-

(defun auth-source-1password--1password-construct-query-path-escaped (_backend _type host user _port)
  "Construct the full entry-path for the 1password entry for HOST and USER.
   Usually starting with the `auth-source-1password-vault', followed
   by host and user."
  (mapconcat #'identity (list auth-source-1password-vault host (string-replace "^" "_" user)) "/"))

;; Mise integration functions
(defun my-mise-which (tool)
  "Get executable path for TOOL from mise, with fallback to global."
  (let ((tool-path (string-trim (shell-command-to-string (format "mise which %s 2>/dev/null" tool)))))
    (if (and tool-path (not (string-empty-p tool-path)) (file-exists-p tool-path))
        tool-path
      tool))) ; fallback to global

;; LSP booster integration
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)
