;;; init-packages.el --- Straight/Use Package -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Straight.el bootstrapping (package.el disabled in early-init)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)  ; Without this we need to add the :straight keyword symbol

(provide 'init-packages)
;;; init-packages.el ends here
