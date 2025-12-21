;;; init-minibuffer.el --- Minibuffer extensions and configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; mu4e doesn't confirm to normal emacs packaging standards so we load it manually. Requires mu to be installed via distro package manager
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-update-interval (* 10 60))

;; Prevents duplicate UID errors
(setf mu4e-change-filenames-when-moving t)

(setq mu4e-maildir-shortcuts
      '(
("/Mailbox/Inbox" . ii)
("/Mailbox/Sent" . ?s)
("/Mailbox/Drafts" . ?d)
("/Mailbox/Archive" . ?a)
	))

(setq mu4e-contexts
      (list
       (make-mu4e-context
	:name "Mailbox"
	:match-func
	(lambda (msg)
	  (when msg
	    (string-prefix-p "/Mailbox" (mu4e-message-field msg :maildir))))
	:vars '((user-mail-address . "ksladowski@mailbox.org")
		(user-full-name    . "Kevin Sladowski")
		(smtpmail-smtp-server . "smtp.mailbox.org")
		(smtpmail-smtp-service . 465)
		(smtpmail-stream-type . ssl)
		(mu4e-drafts-folder  . "/Mailbox/Drafts")
		(mu4e-sent-folder  . "/Mailbox/Sent")
		(mu4e-refile-folder  . "/Mailbox/Archive")
		(mu4e-trash-folder  . "/Mailbox/Trash")))))

(setq message-send-mail-function 'smtpmail-send-it
      mu4e-compose-context-policy 'pick-first
      mu4e-compose-signature "Thanks,\nKevin")

(provide 'init-mu4e)

;;; init-mu4e.el ends here
