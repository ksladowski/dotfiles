;;; init-chezmoi.el --- chezmoi dotfile management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package chezmoi
  :straight
  '( chezmoi
     :host github
     :repo "ksladowski/chezmoi.el"  ; forked the package b/c of a bug that prevents ediff
     :branch "main"
     :files (:defaults "extensions/*")
     :includes (
                chezmoi-ediff
                chezmoi-magit
                chezmoi-dired
                )))

(use-package chezmoi-ediff)

(provide 'init-chezmoi)
;;; init-chezmoi.el ends here
