;;; Out of the box emacs behaviour changes that are not dependent on any
;;; additional packages.

;; Stop the beeping!!!
(setq visible-bell t)

;; Set this right away so all prompts are readable before the full theme is
;; setup.
(set-face-attribute 'minibuffer-prompt nil :foreground "cyan")

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

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

;; Delete all trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace nil t)

;; Set M-x re-builder to use the elisp regexp syntax
;; ref: https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string)
