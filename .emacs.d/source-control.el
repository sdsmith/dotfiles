;;; Source control integration

;; VC is a built-in package that attempts to support all version control
;; systems.
(require 'vc)

(defvar git-gutter+-mode-map)
(defun add-git ()
  ;; ref: https://github.com/nonsequitur/git-gutter-plus
  (require 'git-gutter+)

  ;; NOTE(stewarts): On tty mode with line numbers enabled, the git status
  ;; indicators will shift the line numbers right, truncating them. It is what
  ;; it is.

  (global-git-gutter+-mode)
  (global-set-key (kbd "C-x g") 'git-gutter+-mode) ; toggle in current buffer
  (global-set-key (kbd "C-x G") 'global-git-gutter+-mode) ; Turn on/off globally

  (eval-after-load 'git-gutter+
    '(progn
     ;;; Jump between hunks
       (define-key git-gutter+-mode-map
         (kbd "C-x n") 'git-gutter+-next-hunk)
       (define-key git-gutter+-mode-map
         (kbd "C-x p") 'git-gutter+-previous-hunk)

     ;;; Act on hunks
       (define-key git-gutter+-mode-map (kbd "C-x g v") 'git-gutter+-show-hunk)
       (define-key git-gutter+-mode-map (kbd "C-x g r") 'git-gutter+-revert-hunks)
       ;; Stage hunk at point.
       ;; If region is active, stage all hunk lines within the region.
       (define-key git-gutter+-mode-map (kbd "C-x g t") 'git-gutter+-stage-hunks)
       (define-key git-gutter+-mode-map (kbd "C-x g c") 'git-gutter+-commit)
       (define-key git-gutter+-mode-map
         (kbd "C-x g C") 'git-gutter+-stage-and-commit)
       (define-key git-gutter+-mode-map
         (kbd "C-x g C-y") 'git-gutter+-stage-and-commit-whole-buffer)
       (define-key git-gutter+-mode-map
         (kbd "C-x g U") 'git-gutter+-unstage-whole-buffer)))

  ;; Fringe display for git-gutter+
  ;; ref: https://github.com/nonsequitur/git-gutter-fringe-plus
  ;; Only works in GUI mode (ie. non-tty)
  (when (display-graphic-p)
    (require 'git-gutter-fringe+)))

(defun add-perforce ()
  ;;; Perforce
  ;; Perforce command        Key sequence    Description
  ;; add                     C-x p a         Open file for add.
  ;; annotate                C-x p V         Annotate each line with the revision it was last updated.
  ;; client                  C-x p c         Edit client workspace mapping.
  ;; edit                    C-x p e         Open file for edit.
  ;; delete                  C-x p x         Open file for delete.
  ;; diff                    C-x p =         Diff local file against the depot.
  ;; filelog                 C-x p f         Show revision history of file.
  ;; move                    C-x p m         Move (rename) a file that’s open for edit.
  ;; opened                  C-x p o         List open files.
  ;; reconcile               C-x p z         Reconcile client with workspace changes.
  ;; revert                  C-x p r         Revert file, discarding local changes.
  ;; status                  C-x p s         Identify differences between the workspace and the depot.
  ;; submit                  C-x p S         Submit changes to the depot.
  ;; update                  C-x p g         Get files from depot.
  (require 'p4)
  (add-hook 'fundamental-mode-hook 'perforce-cl-desc-setup))

(defun setup-source-control ()
  (add-perforce)
  (add-git))

(setup-source-control)
