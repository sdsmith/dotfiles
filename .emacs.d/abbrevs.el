(define-abbrev-table 'global-abbrev-table
  '(("0important" "IMPORTANT(stewarts):" nil)
    ("0study" "STUDY(stewarts):" nil)
    ("0note" "NOTE(stewarts):" nil)
    ("0todo" "TODO(stewarts):" nil)
    ("0readme" "README(stewarts):" nil)
    ("0bug" "BUG(stewarts):" nil)
    ("0debug" "DEBUG(stewarts):" nil)
    ("0doc" "DOC(stewarts):" nil)
    ("0war" "WAR():" nil)
    ))

;; Enabling abbrevs for code highlighting
(add-hook 'prog-mode-hook (lambda () (abbrev-mode 1)))

;; (dolist (hook prog-mode-hooks)
;;   (add-hook hook (lambda () (abbrev-mode 1))))
