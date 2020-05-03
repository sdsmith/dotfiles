(defvar org-todo-keywords)

;; Org mode TODO docs
;; http://orgmode.org/manual/Workflow-states.html#Workflow-states
;; http://orgmode.org/manual/Fast-access-to-TODO-states.html#Fast-access-to-TODO-states
;;(setq org-agenda-include-diary t)
;;(setq org-agenda-files "~/.emacs.d/org_mode_agenda_files.txt") ;; TODO: setup

;; put time stamp when tasks are completed
;; ref: https://orgmode.org/guide/Closing-items.html#Closing-items
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "BLOCKED(b@)" "BUG(g)" "|")
        (sequence "|" "POSTPONED(p@)" "CANCELED(c@)" "FIXED(f!)" "DONE(d!)")))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(org-agenda-list)

;; TODO: Create a template for new TODO items
;; - track status changes by default

;; TODO: not working???
(setq org-emphasis-alist
      '(
        ;; Defaults
        ("!" (:foreground "red"))
        ("/" italic)
        ("_" underline)
        ("~" org-code verbatim)
        ("=" org-verbatim verbatim)
        ("+" (:strike-through t))

        ;; Custom
        ("-" (:strike-through t))
        ("`" org-code verbatim)
        ))

