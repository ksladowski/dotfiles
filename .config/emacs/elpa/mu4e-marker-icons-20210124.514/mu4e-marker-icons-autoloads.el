;;; mu4e-marker-icons-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mu4e-marker-icons" "mu4e-marker-icons.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from mu4e-marker-icons.el

(defvar mu4e-marker-icons-mode nil "\
Non-nil if Mu4e-Marker-Icons mode is enabled.
See the `mu4e-marker-icons-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mu4e-marker-icons-mode'.")

(custom-autoload 'mu4e-marker-icons-mode "mu4e-marker-icons" nil)

(autoload 'mu4e-marker-icons-mode "mu4e-marker-icons" "\
Display icons for mu4e markers.

If called interactively, enable Mu4e-Marker-Icons mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mu4e-marker-icons" '("mu4e-marker-icons-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mu4e-marker-icons-autoloads.el ends here
