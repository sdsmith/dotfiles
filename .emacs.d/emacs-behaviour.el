;;; Out of the box emacs behaviour changes that are not dependent on any
;;; additional packages.

;; Stop the beeping!!! and flashing!!!
;; ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Beeping.html
(fset 'ring-bell-function 'ignore)

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

;; Move backup files (.*~) to seperate directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )
(when (not (file-directory-p "~/.emacs.d/backups"))
  (make-directory "~/.emacs.d/backups"))

;; Move autosave files (#.*#) to seperate directory
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves" t)))
(when (not (file-directory-p "~/.emacs.d/autosaves"))
  (make-directory "~/.emacs.d/autosaves"))

(setq inhibit-startup-message t)
(savehist-mode 1)

;; Set fill column
(add-hook 'prog-mode-hook (lambda () (set-fill-column 80)))
(add-hook 'fundamental-mode-hook (lambda () (set-fill-column 80)))

;; Set indentation
;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset tab-width)
(setq cperl-indent-level tab-width)

;; Delete all trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; Set ispell's dictionary location
(setq ispell-personal-dictionary "~/.emacs.d/aspell.en.pws")

;; Swap the command and option keys on OSX
(when (eq system-type 'darwin)
  (setq mac-option-modifer 'alt)
  (setq mac-command-modifier 'meta)
  )
