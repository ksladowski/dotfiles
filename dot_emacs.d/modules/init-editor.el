;;; init-editor.el --- General text edit configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(electric-pair-mode)
(electric-indent-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Spaces not tabs
(setq indent-tabs-mode nil)
(setq tab-width 4)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Debated if there was a better spot for this
(use-package avy
  :general
  (general-define-key
   :states '(normal visual motion)
   :prefix "s"
   "" nil
   "s" 'evil-avy-goto-char-timer
   "j" 'evil-avy-goto-line-below
   "k" 'evil-avy-goto-line-above
   )
  )

(provide 'init-editor)

;;; init-editor.el ends here
