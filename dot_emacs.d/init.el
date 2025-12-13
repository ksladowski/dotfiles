;;; init.el --- small, loads other modules -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'init-packages)
(require 'init-benchmark) ;; Measure startup time, seems to be basically no performance impact of doing this
(require 'init-bindings)
(require 'init-evil)

(use-package emacs
  :config
  (electric-pair-mode)
  (electric-indent-mode)
  (column-number-mode)
  (global-display-line-numbers-mode)
  (set-face-attribute 'default nil :font "Jetbrains Mono Nerd Font" :height 100)
  (set-face-attribute 'variable-pitch nil :font "Inter" :height 100)
  ;; Put backup files neatly away instead of littering all over the file system
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
  :hook
  (before-save-hook . delete-trailing-whitespace)
  :custom
  (visible-bell t)
  (help-window-select t)  ; auto switch to help window upon opening
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)  ; TODO Try `cape-dict' as an alternative.
  (read-extended-command-predicate #'command-completion-default-include-p)  ; Hide commands in M-x which do not apply to the current mode
  ;; Stop littering
  (create-lockfiles nil)  ; Disable lockfiles
  (backup-by-copying t)  ; Don't delink hardlinks
  (delete-old-versions t)  ; Clean up the backups
  (version-control t)  ; Use version numbers on backups,
  (kept-new-versions 5)  ; keep some new versions
  (kept-old-versions 2)  ; and some old ones, too
  ;; Spaces not tabs
  (indent-tabs-mode nil)
  (tab-width 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages (Alphabetical)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ansi-color
  :config
  (defun my/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my/colorize-compilation-buffer))

(use-package ace-window
  :custom
  (aw-dispatch-always t)
  )

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

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t))

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

(use-package consult
  :config
  (defun my/consult-line-to-evil-search (&rest _)
    "Add the latest `consult-line' entry to `evil' search history.
This picks the first whitespace-separated component of the consult-line
pattern (so it's similar to using orderless' first component)."
  (when (and (bound-and-true-p evil-mode)
             (boundp 'consult--line-history))
    (let* ((raw (car consult--line-history))
           (raw (if (and raw (stringp raw)) raw ""))
           ;; pick the first token (like the first orderless component)
           (pattern (car (split-string raw "[[:space:]]+" t))))
      (when (and pattern (> (length pattern) 0))
        (add-to-history 'evil-ex-search-history pattern)
        ;; set current evil search pattern: (PATTERN REGEXP-P FLAG-P)
        (setq evil-ex-search-pattern (list pattern t t))
        (setq evil-ex-search-direction 'forward)
        (when evil-ex-search-persistent-highlight
          (evil-ex-search-activate-highlight evil-ex-search-pattern))))))
  (advice-add #'consult-line :after #'my/consult-line-to-evil-search)
  :general
  (general-nmap "/" 'consult-line)
  (:keymaps 'minibuffer-local-map
            "C-." 'embark-act
            "M-." 'embark-dwim)
  )

(use-package consult-projectile)

(use-package consult-flycheck)
;; (use-package consult-lsp)

(use-package consult-yasnippet)

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

;; (use-package dap-mode
;;   :defer t
;;   :after lsp-mode
;;   :custom
;;   (dap-java-terminal 'integratedTerminal)
;;   :config
;;   (dap-auto-configure-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 5)
  (doom-modeline-hud t)
  (doom-modeline-percent-position '(-3 ""))
  :config
  (doom-modeline-mode 1))

;; set up with freshrss
;; (use-package elfeed)

(use-package embark
  :general
  (general-nmap "C-." 'embark-act)
  (general-nmap "M-." 'embark-dwim)
  (:keymaps 'help-map
            "b" 'embark-bindings)
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )


;; (use-package erc)


(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil)
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

;; TODO flycheck hydra

;; (use-package lsp-mode
;;   :defer t
;;   :commands (lsp lsp-deferred)
;;   :custom
;;   (lsp-completion-providern :none) ;;use corfu
;;  :general
;;   (my/leader-def 'normal
;;     "ll" 'lsp-keymap)
;;   :hook (python-mode . lsp-deferred)
;;   :hook (java-mode . lsp-deferred))

;; (use-package lsp-ui
;;   :after (lsp-mode)
;;   :config
;;   (setq lsp-ui-sideline-enable t
;; 	    lsp-ui-doc-delay 2
;;         lsp-ui-peek-always-show t)
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :general
;;   (my/leader-def 'normal
;;     "ld" 'lsp-ui-peek-find-definitions
;;     "lf" 'lsp-ui-peek-find-references
;;     "lm" 'lsp-ui-imenu))

;; (use-package lsp-treemacs
;;   :after (lsp-mode treemacs)
;;   :commands 'lsp-treemacs-errors-list
;;   :general
;;   (my/leader-def 'normal
;;     "ld" 'lsp-ui-peek-find-definitions
;;     "lf" 'lsp-ui-peek-find-references
;;     "le" 'lsp-treemacs-errors-list))

(use-package magit
  :defer t)

(use-package marginalia
  :init (marginalia-mode))

;; (use-package mu4e)

(use-package orderless
  :init (setq completion-styles '(orderless partial-completion)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :config
  (set-face-attribute 'default nil :font "Jetbrains Mono Nerd Font" :height 100)
  (set-face-attribute 'fixed-pitch nil :family "Jetbrains Mono Nerd Font")

  (set-face-attribute 'variable-pitch nil :family "Inter" :height 1.18)

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
  ;; (org-edit-src-content-indentation 0)
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

(use-package projectile
  :init (projectile-mode 1)
  :custom
  (projectile-project-search-path '("~/src"))
  (projectile-switch-project-action #'projectile-dired)
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)
  (projectile-track-known-projects-automatically nil))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package savehist
  :init (savehist-mode))

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :general
;;   (my/leader-def 'normal
;;     "<tab>" 'treemacs-select-window))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  ;; disable straight for extensions since they are installed with the vertico package. we just need to activate them
  :straight '( vertico :files (:defaults "extensions/*")
               :includes (vertico-buffer
                          vertico-directory
                          vertico-flat
                          vertico-indexed
                          vertico-mouse
                          vertico-quick
                          vertico-repeat
                          vertico-reverse)))

(use-package vertico-directory
  :after vertico
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (yas-global-mode)
  :general
  (my/leader-def 'normal
    "sn" 'yas-new-snippet
    "se" 'yas-visit-snippet-file)
  )

(use-package yasnippet-snippets)

;; (use-package znc)
