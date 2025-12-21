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
(require 'init-ui)
(require 'init-completion)
(require 'init-editor)
(require 'init-environment)
(require 'init-minibuffer)
(require 'init-snippets)
(require 'init-dev)

(require 'init-chezmoi)
(require 'init-org)
(require 'init-mu4e)

;; (use-package consult-lsp)

;; (use-package dap-mode
;;   :defer t
;;   :after lsp-mode
;;   :custom
;;   (dap-java-terminal 'integratedTerminal)
;;   :config
;;   (dap-auto-configure-mode))

;; set up with freshrss
;; (use-package elfeed)

;; (use-package erc)

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

;; (use-package mu4e)

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :general
;;   (my/leader-def 'normal
;;     "<tab>" 'treemacs-select-window))

;; (use-package znc)
