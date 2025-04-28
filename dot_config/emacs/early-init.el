;; -*- lexical-binding: t; -*-

;; Always precompile packages on install instead of on load
(setq-default package-native-compile t)
(setq-default native-comp-jit-compilation t)


(setq ns-use-proxy-icon nil)         ;; Remove icon from title bar

(setq package-enable-at-startup nil) ;; Disable package startup as we will use elpaca instead
(setq frame-resize-pixelwise t)      ;; Whether a frame can be resized by pixel
(tool-bar-mode -1)                   ;; Disable the toolbar

(setq default-frame-alist '((width . (text-pixels . 800))
                            (height . (text-pixels . 600))
                            (font . "Iosevka Nerd Font-12:weight=semi-light")
                            ;; You can turn off scroll bars by uncommenting these lines:
                            ;; (vertical-scroll-bars . nil)
                            ;; (horizontal-scroll-bars . nil)

                            ;; Set as same values from Catppuccin theme to avoid flashes of color during theme activation
                            (background-color . "#303446")
                            (foreground-color . "#c6d0f5")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
