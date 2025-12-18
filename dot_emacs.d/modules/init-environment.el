;;; init-environment.el --- Prevent littering everywhere -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Anti-Littering
(let ((backup-dir "~/.local/share/emacs/backups")
      (auto-saves-dir "~/.local/share/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq create-lockfiles nil)  ; Disable lockfiles
(setq backup-by-copying t)  ; Don't delink hardlinks
(setq delete-old-versions t)  ; Clean up the backups
(setq version-control t)  ; Use version numbers on backups,
(setq kept-new-versions 5)  ; keep some new versions
(setq kept-old-versions 2)  ; and some old ones, too

(provide 'init-environment)
;;; init-environment.el ends here
