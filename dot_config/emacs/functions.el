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
