;; FB: Skip checking the package signatures.
;;
;; NOTE: setting the proxy settings causes package signature checks to
;; fail. Skip those checks. TODO: for now.
(setq package-check-signature nil)

;; Add package sources
(require 'package)

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

;; Skip yes/no prompt, assume yes
(defadvise package-install-selected-packages (around auto-confirm compile activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it)
(package-install-selected-packages)
(ad-unadvise 'package-install-selected-packages)
