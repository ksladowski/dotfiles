;;; init-org.el --- org mode and extensions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :config
  ;; Resize Org headings
  (dolist (face '((org-level-1 . 1.35)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Inter" :weight 'bold :height (cdr face)))
  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font "Inter" :weight
                      'bold :height 1.8)
  ;; Certain elements should always be fixed pitch
  (set-face-attribute 'org-block nil            :foreground nil :inherit
                      'fixed-pitch :height 0.85)
  (set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
  (set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
  (set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)
  :custom
  (org-agenda-files '("~/org"))
  (org-log-done 'time)
  (org-return-follows-link t)
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)  ; bold, italic, etc
  (org-pretty-entities t)  ; render special chars prefixed w/ backslash
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-indentation-per-level 4)
  (org-edit-src-content-indentation 0)
  (org-ellipsis "…")
  (org-capture-templates '(
                           ;; none yet
                           ))
  :hook
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  )

(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

(provide 'init-org)
;;; init-org.el ends here
