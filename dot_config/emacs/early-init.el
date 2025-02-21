;; -*- lexical-binding: t; -*-

(setq ns-use-proxy-icon nil)         ;; Remove icon from title bar

(setq package-enable-at-startup nil) ;; Disable package startup as we will use elpaca instead
(setq frame-resize-pixelwise t)      ;; Whether a frame can be resized by pixel
(tool-bar-mode -1)                   ;; Disable the toolbar

(setq default-frame-alist
  '(
    (width . (text-pixels . 800))
    (height . (text-pixels . 600))
    (font . "Iosevka Nerd Font-12:weight=semi-light")
    ;; You can turn off scroll bars by uncommenting these lines:
    ;; (vertical-scroll-bars . nil)
    ;; (horizontal-scroll-bars . nil)

    ;; Set as same values from Dracula theme to avoid flashes of color during theme activation
    (background-color . "#282a36")
    (foreground-color . "#f8f8f2")
    (ns-appearance . dark)
    (ns-transparent-titlebar . t)))
