;;; init-bindings.el --- general, leader bindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package general
  :after evil  ; Needs to load after evil but the block has to come first so :general is available for undo-tree
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  (general-def "<escape>" 'keyboard-escape-quit) ;; Treat <escape> like C-g
  (general-create-definer my/leader-def
    :states '(normal visual motion)
    :prefix "SPC"
    :global-prefix "C-c")
  (my/leader-def
    "`"  'tmm-menubar
    ":"  'execute-extended-command
    "." 'embark-act
    "<tab>" 'treemacs

    "acf" 'chezmoi-find
    "acd" 'chezmoi-ediff
    "acw" 'chezmoi-write
    "amm" 'mu4e
    "amc" 'mu4e-compose-new

    "bb" 'consult-buffer
    "bk" 'kill-this-buffer

    "h"  'help-command

    "fb" 'consult-bookmark
    "ff" 'find-file
    "fp" 'consult-projectile
    "fd" 'dired

    "p" 'projectile-command-map

    ;; magit
    "g"  'magit-status

    ;; org
    "oa" 'org-agenda
    "oc" 'org-capture
    "ol" 'org-store-link

    ;; snippets / search
    "ss" 'consult-yasnippet
    "sn" 'yas-new-snippet
    "se" 'yas-visit-snippet-file

    ;; undo-tree visualizer (moved from the package stanza into central binds)
    "u"  'undo-tree-visualize

    "w"
    '(lambda () (interactive)
       (setq unread-command-events (listify-key-sequence (kbd "?")))
       (ace-window nil))
    )
  )

(use-package hydra)
(use-package pretty-hydra)

(provide 'init-bindings)
;;; init-bindings.el ends here
