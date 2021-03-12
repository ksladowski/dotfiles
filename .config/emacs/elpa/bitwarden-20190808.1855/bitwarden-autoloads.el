;;; bitwarden-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bitwarden" "bitwarden.el" (0 0 0 0))
;;; Generated autoloads from bitwarden.el

(autoload 'bitwarden-login "bitwarden" "\
Prompts user for password if not logged in.

If run interactively PRINT-MESSAGE gets set and messages are
printed to minibuffer.

\(fn &optional PRINT-MESSAGE)" t nil)

(autoload 'bitwarden-getpass "bitwarden" "\
Get password associated with ACCOUNT.

If run interactively PRINT-MESSAGE gets set and password is
printed to minibuffer.

\(fn ACCOUNT &optional PRINT-MESSAGE)" t nil)

(autoload 'bitwarden-search "bitwarden" "\
Search for vault for items containing SEARCH-STR.

Returns a vector of hashtables of the results.

\(fn &optional SEARCH-STR)" nil nil)

(autoload 'bitwarden-auth-source-enable "bitwarden" "\
Enable Bitwarden auth-source by adding it to `auth-sources'." t nil)

(autoload 'bitwarden-list-all "bitwarden" "\
Show a dialog, listing all entries associated with `bitwarden-user'.
If optional argument GROUP is given, only entries in GROUP will be listed." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bitwarden" '("bitwarden-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bitwarden-autoloads.el ends here
