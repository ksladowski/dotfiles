;;; init-evil.el --- evil modules -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-fine-undo t
        evil-undo-system 'undo-tree
        evil-search-module 'evil-search
        evil-ex-search-highlight-all nil)
  :preface
  ;; Small helpers scoped to this module. Named with init-evil/ prefix to
  ;; avoid clashing with other modules. They don't need to be autoloaded,
  ;; but are defined early so :config can reference them.
  (defun init-evil/kill-buffer-no-star ()
    "Kill this buffer unless its name starts with '*'."
    (interactive)
    (unless (char-equal (elt (buffer-name) 0) ?*)
      (kill-this-buffer)))

  (defun init-evil/save-kill-buffer-no-star ()
    "Save current buffer then kill it (unless its name starts with '*')."
    (interactive)
    (when (buffer-file-name)
      (save-buffer))
    (init-evil/kill-buffer-no-star))
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;; Treat C-g like <escape>
  ;; prefer ex-search motions (works better for consult-line integration)
  (define-key evil-normal-state-map (kbd "n") #'evil-ex-search-next)
  (define-key evil-normal-state-map (kbd "N") #'evil-ex-search-previous)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; Custom ex commands
  (evil-ex-define-cmd "q[uit]" 'init-evil/ex-kill-buffer-and-close)
  (evil-ex-define-cmd "wq" 'init-evil/ex-save-kill-buffer-and-close)
  ;; Special buffer state overrides
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode 1))

(use-package evil-snipe
  :after evil
  :custom
  (evil-snipe-override-mode t)
  (evil-snipe-smart-case t)
  (evil-snipe-repeat-keys t)
  (evil-snipe-override-evil t)
  (evil-snipe-override-evil-repeat-keys 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :general
  (general-mmap undo-tree-visualizer-mode-map
    "<escape>" 'undo-tree-visualizer-abort
    "h" 'undo-tree-visualize-switch-branch-left
    "j" 'undo-tree-visualize-redo
    "k" 'undo-tree-visualize-undo
    "l" 'undo-tree-visualize-switch-branch-right)
  :config
  (global-undo-tree-mode 1)
  ;; undo tree forces itself off if any bindings related to undo are changed. here we override the function to always return nil
  (with-eval-after-load 'undo-tree
    (defun undo-tree-overridden-undo-bindings-p () nil))
  )


(provide 'init-evil)
;;; init-evil.el ends here
