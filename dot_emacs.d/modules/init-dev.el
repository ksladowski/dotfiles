;;; init-dev.el --- dev environment stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defer t)

(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil)
  )
;; TODO flycheck hydra

(use-package consult-flycheck)

(use-package projectile
  :init (projectile-mode 1)
  :custom
  (projectile-project-search-path '("~/src"))
  (projectile-switch-project-action #'projectile-dired)
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)
  (projectile-track-known-projects-automatically nil))

(use-package consult-projectile)

(provide 'init-dev)

;;; init-dev.el ends here
