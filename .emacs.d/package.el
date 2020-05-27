;; FB: Skip checking the package signatures.
;;

;; Add package sources
(require 'package)

;; NOTE: setting the proxy settings causes package signature checks to
;; fail. Skip those checks. TODO: for now.
(setq package-check-signature nil)

;; WAR(https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341): Fixed in emacs 26.3+
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(advice-add 'package-install-selected-packages :around #'disable-y-or-n-p)

(package-install-selected-packages)
