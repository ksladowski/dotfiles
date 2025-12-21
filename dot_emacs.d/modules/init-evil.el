;;; init-evil.el --- evil modules -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil  ; evil-collection handles this
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
	evil-want-C-u-delete t
	evil-want-C-w-delete t
        evil-want-fine-undo t
        evil-undo-system 'undo-redo
        evil-search-module 'evil-search
        evil-ex-search-highlight-all nil)
  :config
  (evil-mode)
  ;; Custom ex commands
  (evil-ex-define-cmd "q[uit]" 'init-evil/kill-buffer-no-star)
  (evil-ex-define-cmd "wq" 'init-evil/save-kill-buffer-no-star)
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
  (evil-goggles-mode))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(provide 'init-evil)
;;; init-evil.el ends here
