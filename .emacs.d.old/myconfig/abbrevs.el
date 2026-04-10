(define-abbrev-table 'global-abbrev-table
  '(("0important" "IMPORTANT(sdsmith):" nil)
    ("0study" "STUDY(sdsmith):" nil)
    ("0note" "NOTE(sdsmith):" nil)
    ("0todo" "TODO(sdsmith):" nil)
    ("0readme" "README(sdsmith):" nil)
    ("0bug" "BUG(sdsmith):" nil)
    ("0debug" "DEBUG(sdsmith):" nil)
    ("0doc" "DOC(sdsmith):" nil)
    ("0war" "WAR():" nil)
    ))

;; Enabling abbrevs for code highlighting
(add-hook 'prog-mode-hook (lambda () (abbrev-mode 1)))

;; (dolist (hook prog-mode-hooks)
;;   (add-hook hook (lambda () (abbrev-mode 1))))
