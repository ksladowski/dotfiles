;;; early-init.el --- Early startup -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor tweaks to improve startup speed and reduce UI flicker on startup.

;;; Code:

;; Increase garbage collection threshold to prevent it from running
;; during startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Temporarily disable file-name-handler-alist for faster startup.
;; We save the original and restore it after init.
(defvar my/orig-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Prevent runtime compilation of lisp code
;; Not strictly necessary since straight compiles packages on install
(setq native-comp-jit-compilation nil)

;; Disable emacs' builtin package management
;; Replaced with straight.el in main init file
(setq package-enable-at-startup nil)

;; Minimal amount of UI changes to avoid flicker on startup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 5)
(setq inhibit-startup-screen t)

;; Silence common lisp deprecation warning
(setq byte-compile-warnings '(cl-functions))

;; Turn everything back on after init
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; restore file-name-handler-alist
            (setq file-name-handler-alist my/orig-file-name-handler-alist)
            ;; restore gc settings (choose conservative defaults)
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)
            (garbage-collect)))

(provide 'early-init)
;;; early-init.el ends here
