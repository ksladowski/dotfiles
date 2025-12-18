;;; init-minibuffer.el --- Minibuffer extensions and configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq read-extended-command-predicate #'command-completion-default-include-p)  ; Hide commands in M-x which do not apply to the current mode

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

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package marginalia
  :init (marginalia-mode))

(use-package savehist
  :init (savehist-mode))

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

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
