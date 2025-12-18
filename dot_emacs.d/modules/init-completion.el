;;; init-completion.el --- Auto complete -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(setq text-mode-ispell-word-completion nil)  ; TODO Try `cape-dict' as an alternative.

(use-package orderless
  :init (setq completion-styles '(orderless partial-completion)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt))

;; TODO dabbrev w/ corfu

;; TODO orderless w/ corfu

;; TODO cape w/ corfu
;; https://github.com/minad/corfu

;; (use-package dabbrev)

(provide 'init-completion)

;;; init-completion.el ends here
