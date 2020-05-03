;;; Out of the box emacs behaviour changes that are not dependent on any
;;; additional packages.

;; Stop the beeping!!!
(setq visible-bell t)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop Emacs from losing undo information by setting very high limits
;; for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; ;; https://stackoverflow.com/questions/10946219/emacs-compilation-mode-wont-see-bash-alias
;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-ic")
;; https://emacs.stackexchange.com/questions/3447/cannot-set-terminal-process-group-error-when-running-bash-script
(setenv "BASH_ENV" "~/.bashrc")

;; Move backup files (*.~) to seperate directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )
(setq autosave-file-name-transforms
      `(("." . "~/.emacs.d/backup")))

(setq inhibit-startup-message t)
(savehist-mode 1)

;; Set fill column
(dolist (hook regular-mode-hooks)
  (add-hook hook (lambda () (set-fill-column 80))))
(add-hook 'fundamental-mode-hook (lambda () (set-fill-column 80)))

;; Set indentation
;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset tab-width)
(setq cperl-indent-level tab-width)

;; Delete all trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace nil t)

;; Set M-x re-builder to use the elisp regexp syntax
;; ref: https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; Set M-x shell settings
(defun my-shell-mode-hook ()
  (process-send-string (get-buffer-process (current-buffer))
                       ;; Set pager to support TERM=dumb when running shell
                       ;; This is necessary for running psql in the shell
                       "export PAGER=/bin/cat\n"))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; Save session defined abbrevs by default
(setq save-abbrevs nil)
