(require 'package)

(setq package-archives '(("org"       . "http://orgmode.org/elpa/") ; define repos
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package helpful 
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package ivy 
:diminish 
:bind (("C-s" . swiper) 
:map ivy-minibuffer-map 
("TAB" . ivy-alt-done) 
("C-l" . ivy-alt-done) 
("C-j" . ivy-next-line) 
("C-k" . ivy-previous-line) 
("C-<RET>" . ivy-immediate-done) 
:map ivy-switch-buffer-map 
("C-k" . ivy-previous-line) 
("C-l" . ivy-done) 
("C-d" . ivy-switch-buffer-kill) 
:map ivy-reverse-i-search-map 
("C-k" . ivy-previous-line) 
("C-d" . ivy-reverse-i-search-kill)) 
:config 
(ivy-mode 1))

(use-package ivy-rich :init (ivy-rich-mode 1)) 
(use-package counsel
:custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only )) 
(use-package swiper)
(use-package all-the-icons-ivy)
(use-package all-the-icons-ivy-rich)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'normal "/" 'swiper)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-goggles
  :config (evil-goggles-mode 1))

(use-package evil-surround
:config (evil-surround-mode 1))

(use-package evil-commentary
:config (evil-commentary-mode 1))

;;sneak emulation
(use-package avy)
(global-set-key (kbd "C-s") 'avy-goto-char-2)

(use-package evil-quickscope)
(add-hook 'prog-mode-hook 'turn-on-evil-quickscope-mode)

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package undo-tree)
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

(global-set-key (kbd "C-M-u") 'universal-argument)

(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(global-unset-key (kbd "C-SPC"))

(use-package general)

(general-create-definer global-definer
  :keymaps 'override
  :states '(insert emacs normal hybrid motion visual operator)
  :prefix "SPC"
  :global-prefix "C-SPC")

(global-definer
  "!"   'shell-command
  ":"   'eval-expression
  "h"   'help-command
  "u"   'universal-argument
  "d"   'dired-jump
  "c"   (general-simulate-key "C-c")
  "x"   (general-simulate-key "C-x")
  "f"   'find-file
  "SPC" 'counsel-linux-app
  "RET" 'vterm)

(defmacro general-global-menu-definer (def infix-key &rest body)
  "Create a definer named general-global-DEF wrapping global-definer.
The prefix map is named 'my-DEF-map'."
  `(progn
     (general-create-definer ,(intern (concat "general-global-" def))
       :wrapping global-definer
       :prefix-map (quote ,(intern (concat "my-" def "-map")))
       :infix ,infix-key
       :wk-full-keys nil
       "" '(:ignore t :which-key ,def))
     (,(intern (concat "general-global-" def))
      ,@body)))

(use-package hydra)
;;Packages will extend these via the general keyword in their own declarations.
(general-global-menu-definer
 "application" "a"
 "a" 'counsel-linux-app
 "x" (general-simulate-key "M-x")
 "m" 'mu4e
 "p" 'proced
 "v" 'vterm
 "f" 'ice/run-firefox
 "g" 'ice/run-lutris
 "d" 'ice/run-discord
 "s" 'ice/run-spotify
 "z" 'ice/run-zoom
 )

(general-global-menu-definer
 "buffer" "b"
 "b"  'ivy-switch-buffer
 "d"  'kill-current-buffer
 "p"  'previous-buffer
 "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
       :which-key "messages-buffer")
 "n"  'next-buffer
 "s" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
       :which-key "scratch-buffer")
 "TAB" '((lambda () (interactive) (switch-to-buffer nil))
         :which-key "other-buffer"))

(defhydra hydra-window ()
   ("h" evil-window-decrease-width "width--")
   ("j" evil-window-decrease-height "height--")
   ("k" evil-window-increase-height "height++")
   ("l" evil-window-increase-width "width++"))
   
(general-global-menu-definer
 "window" "w"
 "h" 'evil-window-left
 "j" 'evil-window-down
 "k" 'evil-window-up
 "l" 'evil-window-right
 "w" 'evil-window-next
 "b" 'ivy-switch-buffer
 "d" 'kill-buffer-and-window
 "s" 'evil-window-split
 "v" 'evil-window-vsplit
 "c" 'evil-window-delete
 "o" 'delete-other-windows
 "f" 'exwm-floating-toggle-floating
 "m" 'exwm-layout-toggle-mode-line
 "r" 'hydra-window/body
 )

(ivy-set-actions
 'ivy-switch-buffer
 '(("v" evil-window-vsplit "vsplit")
   ("s" evil-window-split "split")))

(general-global-menu-definer
 "tab" "t"
 "t" 'tab-new
 "l" 'tab-next
 "h" 'tab-previous
 "r" 'tab-rename
 "c" 'tab-close
 "o" 'tab-close-other
 "n" 'tab-bar-select-tab-by-name
 )

(general-global-menu-definer
    "org" "o"
    "c"   'org-capture
    "i"   'org-insert-link
    "j"   '(:ignore t :which-key "journal")
    ;;just manually specify the prefix key for deeper menus
    "k"   '(:ignore t :which-key "clock")
    "kg"  'org-clock-goto
    "ki"  'org-clock-in-last
    "kj"  'org-clock-jump-to-current-clock
    "ko"  'org-clock-out
    "kr"  'org-resolve-clocks
    "l"   'org-store-link
    "m"   'org-tags-view
    "s"   'org-search-view
    "t"   'org-todo-list)

(defun ice/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun ice/run-firefox ()
    (interactive)
    (ice/run-in-background "firefox")
    (exwm-workspace-switch-create 2))
(defun ice/run-lutris ()
    (interactive)
    (ice/run-in-background "lutris")
    (exwm-workspace-switch-create 4))
(defun ice/run-discord ()
    (interactive)
    (ice/run-in-background "discord")
    (exwm-workspace-switch-create 3))
(defun ice/run-spotify ()
    (interactive)
    (ice/run-in-background "spotify")
    (exwm-workspace-switch-create 2))
(defun ice/run-zoom ()
    (interactive)
    (ice/run-in-background "zoom")
    (exwm-workspace-switch-create 3))

(defun ice/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)

  ;; Launch apps that will run in the background
  (ice/run-in-background "nm-applet")
  (ice/run-in-background "pasystray")
  (ice/run-in-background "flameshot")
  (ice/run-in-background "redshift -l 43.07305:-89.40123")
  (ice/run-in-background "blueman-applet"))

  (add-hook 'exwm-init-hook #'ice/exwm-init-hook )

(defun ice/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))
  

  (add-hook 'exwm-update-class-hook #'ice/exwm-update-class )

(defun ice/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "firefox: %s" exwm-title)))))
    

  (add-hook 'exwm-update-title-hook #'ice/exwm-update-title )

(defun ice/configure-window-by-class ()
  ;;minibuffer message when window launches
  (interactive)
  (message "Window '%s' appeared!" exwm-class-name)

  ;;send windows to specific workspaces
  (pcase exwm-class-name
  ("Firefox" (exwm-workspace-move-window 2))))
  
  ;;can also use float toggle, modeline toggle, etc in here
  

  (add-hook 'exwm-manage-finish-hook #'ice/configure-window-by-class )

;; (use-package desktop-environment
;;   :after exwm
;;   :config (desktop-environment-mode)
;;   :custom
;;   (desktop-environment-brightness-small-increment "2%+")
;;   (desktop-environment-brightness-small-decrement "2%-")
;;   (desktop-environment-brightness-normal-increment "5%+")
;;   (desktop-environment-brightness-normal-decrement "5%-"))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)

 (require 'exwm-randr)
 ; (start-process-shell-command "xrandr" nil "xrandr --output DVI-I-0 --off --output DVI-I-1 --mode 1600x1200 --pos 1920x0 --rotate normal --output HDMI-0 --off --output DP-0 --off --output DVI-D-0 --off --output DP-1 --primary --mode 1920x1200 --pos 0x0 --rotate normal")
 (exwm-randr-enable) 
 
;;theres probably a cleaner way to do this
(setq exwm-randr-workspace-monitor-plist '(1 "DP-1" 
                                           2 "DP-1" 
                                           3 "DP-1" 
                                           4 "DP-1" 
                                           5 "DP-1" 
                                           6 "DVI-I-1"
                                           7 "DVI-I-1"
                                           8 "DVI-I-1"
                                           9 "DVI-I-1"
                                           0 "DVI-I-1"))

  ;; Window focus should follow the mouse pointer
  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  ;;needs to be loaded before exwm-init
(require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 32)
  (exwm-systemtray-enable)

  ;;always accept these keys
(setq exwm-input-prefix-keys
    '(?\M-`
      ?\C-\s))  ;; Ctrl+Space
      
;;next input will be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;;basically only use super keys here
(setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-i] . exwm-input-release-keyboard)
          ([?\s-`] . exwm-reset)
          
          ;; Launch applications via shell command
          ([?\s-\ ] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

          ;; Automatically move EXWM buffer to current workspace when selected
          (setq exwm-layout-show-all-buffers t)
          ;; Display all EXWM buffers in every workspace buffer list
          (setq exwm-workspace-show-all-buffers t)


  (exwm-enable))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>]❯ *")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(global-set-key (kbd "C-p") 'vterm-yank)

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config

  ;;req'd for capture template
  (require 'mu4e-org)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)
  
  ;; Emacs' built in smtp
  (setq message-send-mail-function 'smtpmail-send-it)
  
  ;; Refresh mail using isync every 15 minutes
  (setq mu4e-update-interval (* 15 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-context-policy 'pick-first)

  ;; Formatting for other clients. If they don't support this, it does nothing. No downside
  (setq mu4e-compose-format-flowed t)
  
  ;; this next block fixes delete functionality
 (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N")))) 
                                           

  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)
  (setq mu4e-compose-dont-reply-to-self t)

  (setq mu4e-contexts
      (list
        ;; Personal account
        (make-mu4e-context
        :name "Slad"
        :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Slad" (mu4e-message-field msg :maildir))))
              :vars '((user-mail-address . "kevm@sladowski.us")
                (user-full-name    . "Kevin Sladowski")
                (smtpmail-smtp-user  . "kevm@sladowski.us")
                (smtpmail-smtp-server  . "smtpout.secureserver.net")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/Slad/Drafts")
                (mu4e-sent-folder  . "/Slad/Sent")
                (mu4e-refile-folder  . "/Slad/Archive")
                (mu4e-trash-folder  . "/Slad/Trash")))

        ;; Wisc account
        (make-mu4e-context
        :name "Wisc"
        :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Wisc" (mu4e-message-field msg :maildir))))
              :vars '((user-mail-address . "sladowski@wisc.edu")
                (user-full-name    . "Kevin Sladowski")
                (smtpmail-smtp-user  . "sladowski@wisc.edu")
                (smtpmail-smtp-server  . "smtp.office365.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/Wisc/Drafts")
                (mu4e-sent-folder  . "/Wisc/Sent Items")
                (mu4e-refile-folder  . "/Wisc/Archive")
                (mu4e-trash-folder  . "/Wisc/Deleted Items")))

         ;; Gmail account
         (make-mu4e-context
          :name "Gmail"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
                :vars '((user-mail-address . "kevs1198@gmail.com")
                  (user-full-name    . "Kevin Sladowski")
                  (smtpmail-smtp-user  . "kevs1198@gmail.com")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")))))

(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "All Inboxes"
        :query "maildir:/Wisc/INBOX OR maildir:/Gmail/INBOX OR maildir:/Slad/INBOX"
        :key ?i)
       (make-mu4e-bookmark
        :name "All Archives"
        :query "maildir:/Wisc/Archive OR maildir:/Gmail/[Gmail]/All Mail OR maildir:/Slad/Archive"
        :key ?a))


)

(use-package mu4e-marker-icons
  :init (mu4e-marker-icons-mode 1))

(use-package password-store)
(use-package pass)
(use-package auth-source-pass)
(auth-source-pass-enable)
(use-package ivy-pass)
(global-definer "p" 'ivy-pass)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-switch-project-action #'projectile-dired )
  ;; these two lines are req'd for the globally ignored directories
  (setq projectile-enable-caching t)

  (setq projectile-indexing-method 'native)
  (setq projectile-globally-ignored-directories
    '(".git"
    ".ccls-cache"
    ".svn"
    ".stack-work"
    ".cquery_cached_index")))

(global-definer
    "P" 'projectile-command-map)

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

;; (use-package forge
;;   :after magit)

(general-global-menu-definer
 "git" "g"
  "g" 'magit-file-dispatch
  "c" 'magit-clone)

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package treemacs)
(use-package treemacs-magit)
(use-package treemacs-evil)

(defun ice/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . ice/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))
  
(global-definer "l" 'lsp-keymap-prefix)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection)
         ("C-j" . company-select-previous)
         ("C-k" . company-select-next))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ivy)
(use-package lsp-treemacs
  :after lsp)
(lsp-treemacs-sync-mode 1)

(use-package ccls
  :config
  (setq ccls-executable "ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package dap-mode
:config
  ;; Set up Node
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package gdb-mi)
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gdb-show-changed-values t)
(setq gdb-use-colon-colon-notation t)
(setq gdb-use-separate-io-buffer nil)
(setq gdb-delete-out-of-scope t)
(setq gdb-speedbar-auto-raise t)

(defadvice gdb-setup-windows (after activate)
  (gdb-setup-my-windows)
)

(defun gdb-setup-my-windows ()
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let
    ((win0 (selected-window))             ; breakpoints
     (win1 (split-window-horizontally
         (floor (* 0.5 (window-width)))))   ; source + i/o
     (win2 (split-window-vertically
         (floor (* 0.5 (window-body-height))))) ; gdb
     (win3 (split-window-vertically
        (floor (* 0.5 (window-body-height))))) ; locals
     (win4 (split-window-vertically
         (floor (* 0.6 (window-body-height))))) ; stack
    )
    (select-window win1)
    ; configurating right window
    (let
    ((winSrc (selected-window)) ; source
     (winIO (split-window-vertically (floor (* 0.9 (window-body-height))))) ; I/O
     )
      (set-window-buffer winIO (gdb-get-buffer-create 'gdb-inferior-io))
      (set-window-buffer
    winSrc
    (if gud-last-last-frame
     (gud-find-file (car gud-last-last-frame))
      (if gdb-main-file
       (gud-find-file gdb-main-file)
     (list-buffers-noselect))))
      (setq gdb-source-window winSrc)
      (set-window-dedicated-p winIO t)
   )

    (set-window-buffer win0 (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-buffer win3 (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-buffer win4 (gdb-get-buffer-create 'gdb-stack-buffer))
    (select-window win2)
  )
)

(add-to-list 'display-buffer-alist
         (cons 'gdb-source-code-buffer-p
           (cons 'display-buffer-use-some-window nil)))

(defun gdb-source-code-buffer-p (bufName action)
  (let ((buf (get-buffer bufName)))
    (and buf
         (eq gud-minor-mode 'gdbmi)
         (with-current-buffer buf
           (derived-mode-p buf 'c++-mode 'c-mode)))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-face-attribute 'default nil :font "JetBrains Mono Nerd Font" :height 100)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
      treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq fast-but-imprecise-scrolling nil)
(setq jit-lock-defer-time 0)
(setq scroll-margin 4)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-inhibit-click-time nil)
(setq scroll-preserve-screen-position t)
(setq scroll-error-top-bottom t)
(setq next-error-recenter (quote (4)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
(setq make-pointer-invisible t)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

(use-package doom-themes
  :init (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq-default indicate-empty-lines t)
(setq bidi-display-reordering nil)
(setq cursor-in-non-selected-windows 'hollow)

;; hide clickable buttons
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq dashboard-banner-logo-title "icEmacs -- kevin@icebox")
(setq dashboard-center-content t)

(setq dashboard-set-navigator t)

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-footer nil)
(setq dashboard-week-agenda t)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))

(setq select-enable-clipboard t)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(fset 'undo-auto-amalgamate 'ignore)
(setq undo-limit 6710886400)
(setq undo-strong-limit 100663296)
(setq undo-outer-limit 1006632960)

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-directory-alist `(("." . "~/.config/emacs/backups")))
(setq vc-follow-symlinks t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 4)
(setq tab-width 4)
;; always use spaces
(progn 
  (setq-default indent-tabs-mode nil))
  ;; try to indent, otherwise complete
(setq-default tab-always-indent 'complete)
(electric-indent-mode nil)

(savehist-mode 1)
(setq savehist-additional-variables '(register-alist))

(define-key key-translation-map (kbd "<ESC>") (kbd "C-g"))

(use-package org
  :hook (org-mode . ice/org-mode-setup)
  :config
  (setq org-ellipsis " ▾" ; character to use to show heading can be unfolded
	org-hide-emphasis-markers t))

(defun ice/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(defun ice/org-babel-tangle-dont-ask ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ice/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))

(use-package org-mime)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-agenda-files '("~/documents/org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'note)
(setq org-log-into-drawer t)
(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))

(setq org-agenda-custom-commands
 '(("d" "Dashboard"
   ((agenda "" ((org-deadline-warning-days 7)))
    (todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))

  ("n" "Next Tasks"
   ((todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))))

  ;; tag sort example
  ("W" "Work Tasks" tags-todo "+work-email")))))

(setq org-capture-templates
    `(("t" "Task" entry (file+olp "~/documents/org/todo.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("a" "Appointment" entry (file+olp "~/documents/org/todo.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("m" "Mail" entry (file+olp "~/documents/org/todo.org" "Inbox")
          "* TODO %a")
      ("d" "Deadline" entry (file+headline as/gtd "")
            "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
      ("j" "Journal" entry (file+datetree "~/documents/org/journal.org")
        "* %? %^G\nEntered on %U\n")))

(setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("ECE 271" . ?l)
       ("ECE 340" . ?c)
       ("ECE 303" . ?L)
       ("ECE 434" . ?p)
       ("CS 506" . ?s)
       ("CS 537" . ?o)
       ("note" . ?n)
       ("idea" . ?i)))

(setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("todo.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)