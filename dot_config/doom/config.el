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

;; Set hooks to load mise globally
(add-hook 'after-init-hook #'global-mise-mode)

;; Disable autoformat on HTML files
(add-to-list '+format-on-save-disabled-modes 'html-mode)

;; Enable word wrap (almost) everywhere
(+global-word-wrap-mode +1)

;; Configure eglot LSPs
(after! eglot
  (add-to-list 'eglot-server-programs
               `(typescript-mode . ("typescript-language-server" "--stdio" :initializationOptions '(:importModuleSpecifierPreference "relative"))))
               `(json-mode . ("vscode-json-language-server")))

;; Configure copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; Configure apheleia linters
(after! apheleia
  (add-to-list 'apheleia-mode-alist '(ruby-mode . (rubocop))))

;; Remove icon from title bar
(setq ns-use-proxy-icon nil)

;; Fix path variable availability in MacOS
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Force Emacs to use a POSIX shell
(setq shell-file-name (executable-find "bash"))
;; Switch vterm to fish shell
(setq vterm-shell (executable-find "fish"))
(setq explicit-shell-file-name (executable-find "fish"))

;; 1Password integration
(defun auth-source-1password--1password-construct-query-path-escaped (_backend _type host user _port)
  "Construct the full entry-path for the 1password entry for HOST and USER.
   Usually starting with the `auth-source-1password-vault', followed
   by host and user."
  (mapconcat #'identity (list auth-source-1password-vault host (string-replace "^" "_" user)) "/"))

(use-package! auth-source-1password
  :custom
  (auth-source-1password-vault "Private")
  (auth-source-1password-construct-secret-reference #'auth-source-1password--1password-construct-query-path-escaped)
  :config
  (auth-source-1password-enable))

;; gptel OpenRouter configuration
(defun gptel-get-openrouter-api-key ()
  "Retrieves the OpenRouter API key from 1Password"
  (auth-source-pick-first-password :host "OpenRouter" :user "API Token"))

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

;; Enforce dbml-mode on .dbml files
(use-package! dbml-mode
  :config
  (add-to-list 'auto-mode-alist
             '("\\.dbml\\'" . dbml-mode)))

;; Add a keybind to gptel-menu
(map! "C-c g" #'gptel-menu)
(map! :leader :desc "Pop up gptel menu" "l" #'gptel-menu)
