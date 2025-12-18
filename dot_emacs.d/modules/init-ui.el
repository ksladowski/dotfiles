;;; init-ui.el --- UI configuration and theming -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(column-number-mode)
(global-display-line-numbers-mode)

(set-face-attribute 'default nil :font "Jetbrains Mono Nerd Font" :height 100)
(set-face-attribute 'fixed-pitch nil :family "Jetbrains Mono Nerd Font")
(set-face-attribute 'variable-pitch nil :family "Inter" :height 1.18)

(setq visible-bell t)

(use-package ansi-color
  :config
  (defun my/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook
  (compilation-filter . my/colorize-compilation-buffer))

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t))

(use-package doom-modeline
  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 5)
  (doom-modeline-hud t)
  (doom-modeline-percent-position '(-3 ""))
  :config
  (doom-modeline-mode 1))

;; maybe these should be in a "windows" module?
(use-package ace-window
  :custom
  (aw-dispatch-always t)
  )

  (setq help-window-select t)  ; auto switch to help window upon opening

(provide 'init-ui)

;;; init-ui.el ends here
