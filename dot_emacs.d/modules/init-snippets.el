;;; init-snippets.el --- Prevent littering everywhere -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (yas-global-mode)
  )

(use-package yasnippet-snippets)

(use-package consult-yasnippet)

(provide 'init-snippets)

;;; init-snippets.el ends here
