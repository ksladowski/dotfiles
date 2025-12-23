;;; init-ui.el --- UI configuration and theming -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(column-number-mode)
(display-line-numbers-mode)

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

;; (use-package catppuccin-theme
;;   :config
;;   (load-theme 'catppuccin t))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-gruvbox")
  :config
  (load-theme 'doom-gruvbox)
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config)
  )

(use-package solaire-mode
  :config
  (solaire-global-mode)
)

(use-package doom-modeline
  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 5)
  (doom-modeline-hud t)
  (doom-modeline-percent-position '(-3 ""))
  :config
  (doom-modeline-mode))

(use-package hide-mode-line)

(use-package treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
	treemacs-is-never-other-window t
	treemacs-sorting 'alphabetic-case-insensitive-asc  ; case sensitive by default
	)
  :config
  (treemacs-follow-mode nil)
  (setq treemacs-user-mode-line-format 'none)
  )

;; unicode symbols
(use-package pretty-mode
  :init
  (global-pretty-mode))

;;; Windows (not the os)

;; maybe these should be in a "windows" module?
(use-package ace-window
  :custom
  (aw-dispatch-always t)
  (defvar aw-dispatch-alist
    '(
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-delete-window "Close Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?s aw-split-window-vert "Split Horz Window")
      (?v aw-split-window-horz "Split Vert Window")
      (?o delete-other-windows "Delete Other Windows")
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  )

(setq help-window-select t)  ; auto switch to help window upon opening

(provide 'init-ui)

;;; init-ui.el ends here
