;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 12 :weight 'semi-light))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-pro)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Set tabs to spaces
(setq-default indent-tabs-mode nil)
;; Set tab to two spaces
(setq-default tab-width 2)
(setq-default smie-indent-basic 2)
(setq-default c-basic-offset 2)
(setq-default sh-basic-offset 2)
(setq-default standard-indent 2)
(setq-default typescript-indent-level 2)

;; Remove icon from title bar
(setq ns-use-proxy-icon nil)

;; Fix path variable availability in MacOS
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Force Emacs to use a POSIX shell
(setq shell-file-name (executable-find
                       "bash"))
;; Switch vterm to fish shell
(setq vterm-shell (executable-find
                   "fish"))

;; Authentication configurations
(use-package! auth-source-pass)
(auth-source-pass-enable)

;; gptel OpenRouter configuration
(setq gptel-openrouter-api-key nil) ; Initialize the cache variable
(setq gptel-openrouter-api-key-expiration 0) ; Initialize expiration time

(defun gptel-get-openrouter-api-key ()
  "Retrieves the OpenRouter API key from 1Password, caching it for 30 minutes."
  (let ((current-time (float-time)))
    (cond
     ((or (not gptel-openrouter-api-key)
          (< gptel-openrouter-api-key-expiration current-time))
      ;; Key is not cached or has expired
      (message "Fetching OpenRouter API key from 1Password...")
      (let ((api-key (string-trim (shell-command-to-string "op read \"op://Private/3wd65tvsf4vpc2gm2564lj5yky/credential\""))))
        (setq gptel-openrouter-api-key api-key)
        (setq gptel-openrouter-api-key-expiration (+ current-time (* 30 60))) ; Cache for 30 minutes (30 * 60 seconds)
        api-key))
     (t
      ;; Key is cached and valid
      gptel-openrouter-api-key))))

(setq gptel-model   'deepseek/deepseek-r1:free
      gptel-backend
      (gptel-make-openai "OpenRouter"               ;Any name you want
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key #'gptel-get-openrouter-api-key ; Use the caching function
        :models '(deepseek/deepseek-r1:free
                  deepseek/deepseek-r1
                  openai/gpt-3.5-turbo
                  mistralai/mixtral-8x7b-instruct
                  meta-llama/codellama-34b-instruct
                  codellama/codellama-70b-instruct
                  google/gemini-exp-1206:free
                  google/gemini-2.0-flash-exp:free
                  google/gemini-2.0-flash-thinking-exp:free)))
