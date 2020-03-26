;;; .emacs Configuration file
;;
;; TODO(sdsmith):
;; - project management, including TODO file(s) and comment management
;; - insert biolerplate into files
;; - emacs deamon (server-start)
;;   - run emacs --daemon from .profile
;;   - use emacs client to open a file
;; TODO(stewarts): don't use setq! https://emacs.stackexchange.com/questions/17386/display-all-setq-possibilities

;; Tips
;;
;; - describe-face: Describes font face. Defaults to face at point.
;; - ibuffer: nice table of open buffers

;; FB: setup proxy to talk to internet
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "fwdproxy:8080")
             ("https" . "fwdproxy:8080")))

;; FB: Skip checking the package signatures.
;;
;; NOTE: setting the proxy settings causes package signature checks to
;; fail. Skip those checks. TODO: for now.
(setq package-check-signature nil)


;; Stop the beeping!!!
(setq visible-bell t)

;; Set this right away so all prompts are readable
(set-face-attribute 'minibuffer-prompt nil :foreground "cyan")

;; Setup customize system
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; NOTE(sdsmith): ALWAYS DO THIS BEFORE ANY PACKAGE CUSTOMIZATION
(defun setup-packages ()
  "Set the package archives to search for packages."
  ;; Add package sources
  (require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
    (add-to-list 'package-archives (cons "melpa" url) t))
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install-selected-packages))
(setup-packages)

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

;; Change window configuration (C-c Left|Right to navigate history)
(when (fboundp 'winner-mode)
  (winner-mode 1))
  ;; (global-set-key (kbd "M-<up>") 'enlarge-window)
  ;; (global-set-key (kbd "M-<down>") 'shrink-window)
  ;; (global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
  ;; (global-set-key (kbd "M-<right>") 'enlarge-window-horizontally))

(setq inhibit-startup-message t)
(savehist-mode 1)

;; ;; https://stackoverflow.com/questions/10946219/emacs-compilation-mode-wont-see-bash-alias
;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-ic")
;; https://emacs.stackexchange.com/questions/3447/cannot-set-terminal-process-group-error-when-running-bash-script
(setenv "BASH_ENV" "~/.bashrc")

;; line-move-partial is the scrolling lag scurge of the universe. This causes line-move to skip calling line-move-partial. When compiling MODS in a compilation buffer line-move-partial was responsible for _90%_ of the execution time. And it was laggy. Multiple seconds responce laggy. Kill the demon.
;; ref: https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)

;; Set M-x re-builder to use the elisp regexp syntax
;; ref: https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; Stopping the emacs server
(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;; MacOS clipboard integration
;; ref: http://iancmacdonald.com/macos/emacs/tmux/2017/01/15/macOS-tmux-emacs-copy-past.html
;; NOTE: requires reattach-to-user-namespace
;; (https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard)
(defun copy-from-osx ()
  "Use OSX clipboard to paste."
  (shell-command-to-string "reattach-to-user-namespace pbpaste"))

(defun paste-to-osx (text &optional push)
  "Add kill rinf entries (TEXT) to OSX clipboard. PUSH."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "reattach-to-user-namespace" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(if (eq system-type 'darwin)
    (progn
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx)))

;;; Perforce integration
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

;;; Globals
;; Set of regularly used modes
(setq regular-modes
      '(c++-mode
        c-mode
        csharp-mode
        emacs-lisp-mode
        python-mode
        sql-mode
        java-mode
        php-mode
        js-mode
        488-lang-mode
        asm-mode))

(setq regular-mode-hooks
      '(c++-mode-hook
        c-mode-hook
        csharp-mode-hook
        emacs-lisp-mode-hook
        python-mode-hook
        sql-mode-hook
        java-mode-hook
        php-mode-hook
        js-mode-hook
        488-lang-mode-hook
        asm-mode-hook))

;; Set fill column
(dolist (hook regular-mode-hooks)
  (add-hook hook (lambda () (set-fill-column 80))))
(add-hook 'fundamental-mode-hook (lambda () (set-fill-column 80)))

;; Display the current function in the mode line
(dolist (hook regular-mode-hooks)
  (add-hook hook (lambda () (which-function-mode))))
;; TODO: Set the font face of face `which-func`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NVIDIA Stuff
;;
;; (defconst nvidia-c-style
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Adding-Styles.html#Adding-Styles
;;   '(linux
;;     (
;;
(defun nvidia-c-setup ()
  ;; Indentation
  (c-set-offset 'inline-open 0)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label 4)

  ;; ;; Add doxygen comments to comment-start-skip
  ;; (setq comment-start-skip "\\(//+\\|//!\\|/\\*+\\)\\s *")
  )
(add-hook 'js-mode-hook 'nvidia-c-setup)
(add-hook 'c-mode-hook 'nvidia-c-setup)
(add-hook 'c++-mode-hook 'nvidia-c-setup)
(defun del-t-on-save ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'js-mode-hook #'del-t-on-save)
(add-hook 'c-mode-hook #'del-t-on-save)
(add-hook 'c++-mode-hook #'del-t-on-save)

(require 'highlight-doxygen)
(add-hook 'c++-mode-hook 'highlight-doxygen-mode)
(add-hook 'c-mode-hook 'highlight-doxygen-mode)
(add-hook 'js-mode-hook 'highlight-doxygen-mode)
(set-face-attribute 'highlight-doxygen-comment nil :foreground "grey60" :background "grey20")
(set-face-attribute 'highlight-doxygen-variable nil :foreground "grey80")

;; TODO: make interactive
(defun split-string-every (string chars)
  "Split STRING into substrings of length CHARS characters.

This returns a list of strings"
  (cond ((string-empty-p string)
         nil)
        ((< (length string)
            chars)
         (list string))
        (t (cons (substring string 0 chars)
                 (split-string-every (substring string chars)
                                     chars)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; TypeScript - Tide mode

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ;; format options
;; (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; (add-hook 'js2-mode-hook #'setup-tide-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar git-gutter+-mode-map)
(defun add-git-gutter-plus ()
  ;; https://github.com/nonsequitur/git-gutter-plus
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
       (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
       (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
       ;; Stage hunk at point.
       ;; If region is active, stage all hunk lines within the region.
       (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
       (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
       (define-key git-gutter+-mode-map
         (kbd "C-x C") 'git-gutter+-stage-and-commit)
       (define-key git-gutter+-mode-map
         (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
       (define-key git-gutter+-mode-map
         (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))))

(defun add-git-gutter ()
     (require 'git-gutter)
     (global-git-gutter-mode t))


;; ;; https://github.com/jordonbiondo/column-enforce-mode/
;; (add-to-list 'load-path  "~/.emacs.d/manual-packages/column-enforce-mode/")
;; (require 'column-enforce-mode)
;; ;;(add-hook 'prog-mode-hook 'column-enforce-mode)
;; (setq column-enforce-column 80)

;;; Fix annoying things
;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; ;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

(defun configure-emacs ()
  "Configure various emacs settings."
  (progn
    ;; Stop Emacs from losing undo information by setting very high limits
    ;; for undo buffers
    (setq undo-limit 20000000)
    (setq undo-strong-limit 40000000)

    ;; Set M-x shell settings
    (add-hook 'shell-mode-hook 'my-shell-mode-hook)
    (defun my-shell-mode-hook ()
      (process-send-string (get-buffer-process (current-buffer))
                           ;; Set pager to support TERM=dumb when running shell
                           ;; This is necessary for running psql in the shell
                           "export PAGER=/bin/cat\n"))

    ;; Highlight current line
    (global-hl-line-mode 1)
    (set-face-background 'highlight
                                        ; "#232323" ; dark gray
                                        ; "#b22222" ; red pop
                                        ; "#8b1a1a" ; deep red
                         "#8b0000"
                         )

    (set-face-background 'isearch-fail "red")
    (set-face-background 'region "blue")

    ;; Switching windows easier navigation
    ;; S-<left>, S-<right>, S-<up>, S-<down>
    (windmove-default-keybindings)

    ;; Show file path in status bar when using non-unqiue names
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

    ;; Show full file path as window title
    (setq frame-title-format
          (list (format "%s %%S: %%j " (system-name))
                '(buffer-file-name "%f"
                                   (dired-directory dired-directory "%b"))))

    ;; Save session defined abbrevs by default
    (setq save-abbrevs nil)

    ;; Display current column number in buffer status bar
    (setq column-number-mode t)

    ;; Set indentation
    ;; Use spaces, not tabs
    (setq-default indent-tabs-mode nil)
    (setq tab-width 4)
    (setq c-basic-offset tab-width)
    (setq cperl-indent-level tab-width)))

(defun set-abbrev-table ()
  "Define the abbrev table."
  ;; Define default abbrevs
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
      )))


(defun configure-syntax ()
  "Configure syntax and highlighting."
  (progn
    ;; Additional Highlighting
    (make-face 'font-lock-comment-user-face)
    (make-face 'font-lock-comment-todo-face)
    (make-face 'font-lock-comment-note-face)
    (make-face 'font-lock-comment-important-face)
    (make-face 'font-lock-comment-study-face)
    (make-face 'font-lock-comment-readme-face)
    (make-face 'font-lock-comment-bug-face)
    (make-face 'font-lock-comment-debug-face)
    (make-face 'font-lock-comment-doc-face)
    (make-face 'font-lock-comment-war-face)
    (make-face 'font-lock-comment-bug-ref-face)

    ;; TODO(stewarts): add doxygen comment highlighting
    (mapc (lambda (mode)
            (font-lock-add-keywords
             mode
             '(
               ("\\<\\(TODO(\\w+?):\\)" 1 'font-lock-comment-todo-face t)
               ("\\<\\(NOTE(\\w+?):\\)" 1 'font-lock-comment-note-face t)
               ("\\<\\(IMPORTANT(\\w+?):\\)" 1 'font-lock-comment-important-face t)
               ("\\<\\(STUDY(\\w+?):\\)" 1 'font-lock-comment-study-face t)
               ("\\<\\(README(\\w+?):\\)" 1 'font-lock-comment-readme-face t)
               ("\\<\\(BUG(\\w+?):\\)" 1 'font-lock-comment-bug-face t)
               ("\\<\\(DEBUG(\\w+?):\\)" 1 'font-lock-comment-debug-face t)
               ("\\<\\(DOC(\\w+?):\\)" 1 'font-lock-comment-doc-face t)
               ("\\<\\(WAR(\\w+?):\\)" 1 'font-lock-comment-war-face t)
               ("\\(TODO\\|NOTE\\|IMPORTANT\\|STUDY\\|README\\|BUG\\|DOC\\)(\\(\\w+?\\)):"
                2 'font-lock-comment-user-face t)
               ("WAR(\\(\\w+?\\)):" 1 'font-lock-comment-bug-ref-face t)
               )))
          regular-modes)

    (modify-face 'font-lock-comment-user-face "thistle4" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-todo-face "Red3" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-note-face "DarkOliveGreen" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-important-face "gold" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-study-face "gold" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-readme-face "DodgerBlue" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-bug-face "chartreuse" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-debug-face "chartreuse" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-doc-face "DeepPink2" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-war-face "Red3" nil nil t nil nil nil nil)
    (modify-face 'font-lock-comment-bug-ref-face "chartreuse" nil nil t nil nil nil nil)

    ;; Enabling abbrevs for code highlighting
    (dolist (hook regular-mode-hooks)
      (add-hook hook (lambda () (abbrev-mode 1))))

    ;; Add keywords to cpp
    (font-lock-add-keywords 'c++-mode
                            '(("constexpr" . font-lock-keyword-face))
                            '(("nullptr"   . font-lock-keyword-face)))

    ;; Load major modes
    (autoload 'glsl-mode "glsl-mode" nil t) ; OpenGL Shader Language
    (add-to-list 'load-path  "~/.emacs.d/488-source-lang/")
    ;; TODO(sdsmith): include all references to this lib here?
    (when (require '488-lang-mode nil 'noerror)) ; CSC488 Source Language (Winter 2016)
    (require 'json-mode)
    (require 'glsl-mode)
    (require 'asm-mode)

    ;; Associate file extentions and their appropriate modes
    (setq auto-mode-alist
          (append
           '(("\\.cpp$"           . c++-mode)
             ("\\.h$"             . c++-mode)
             ("\\.c$"             . c++-mode)
             ("\\.txt$"           . indented-text-mode)
             ("\\.emacs$"         . emacs-lisp-mode)
             ("\\.vert$'"         . glsl-mode)
             ("\\.frag$'"         . glsl-mode)
             ("\\.cmake$"         . cmake-mode)
             ("CMakeLists\\.txt$" . cmake-mode)
             ("\\.el$"            . emacs-lisp-mode)
             ("\\.org$"           . org-mode)
             ("\\.org.gpg$"       . org-mode)
             ("\\.ref$"           . org-mode)
             ("\\.ref.gpg$"       . org-mode)
             ("\\.notes$"         . org-mode)
             ("\\.js$"            . js-mode)
             ("\\.json$"          . json-mode)
             ("makedefs\\.inc$"   . makefile-gmake-mode)
             ("makesrc\\.inc$"    . makefile-gmake-mode)
             ("Makefile\\.*$"     . makefile-gmake-mode)
             ("make.+\\.inc$"     . makefile-gmake-mode)
             ("\\.spc$"           . js-mode)
             ("\\.nvasm$"         . asm-mode)
             ("\\.gdb$"           . gdb-script-mode)
             ("\\.bashrc$"        . sh-mode)
             ("\\.bashrc_.+$"     . sh-mode)
             ) auto-mode-alist))

  (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
  (setq auto-mode-alist
     (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

    ;; Default syntax style
    (setq c-default-style "linux")

    ;; Custom text (For windowed mode)
    (add-to-list 'default-frame-alist '(font . "Liberation Mono-9.5"))
    ;; (set-face-attribute 'default t :font "Liberation Mono-10") ;; BUG(sdsmith): causes 'emacs --daemon' to error during startup
    (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
    (set-face-attribute 'font-lock-comment-face nil :foreground "gray60")
    (set-face-attribute 'font-lock-constant-face nil
                        :foreground "DarkKhaki")
    (set-face-attribute 'font-lock-doc-face nil :foreground "burlywood3")
    (set-face-attribute 'font-lock-function-name-face nil
                        :foreground "IndianRed1")
    (set-face-attribute 'font-lock-keyword-face nil
                        :foreground "DarkGoldenrod3")
    (set-face-attribute 'font-lock-string-face nil
                        :foreground "LightYellow1")
    (set-face-attribute 'font-lock-type-face nil
                        :foreground "IndianRed1")
    (set-face-attribute 'font-lock-variable-name-face nil
                        :foreground "burlywood3")
    (set-face-attribute 'font-lock-preprocessor-face nil
                        :foreground "MediumPurple1")
    (set-face-attribute 'default nil
                        :foreground "burlywood3")

    ;; Original config
    ;;
    ;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-9.5"))
    ;; ;; (set-face-attribute 'default t :font "Liberation Mono-10") ;; BUG(sdsmith): causes 'emacs --daemon' to error during startup
    ;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
    ;; (set-face-attribute 'font-lock-comment-face nil :foreground "gray60");;"dark grey");;"thistle4" ;;"gray50")
    ;; (set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
    ;; (set-face-attribute 'font-lock-doc-face nil :foreground "burlywood3")
    ;; (set-face-attribute 'font-lock-function-name-face nil
    ;;                                     ; :foreground "DarkGoldenrod3")
    ;;                                     ; :foreground "IndianRed1")
    ;; (set-face-attribute 'font-lock-keyword-face nil
    ;;                     :foreground "DarkGoldenrod3")
    ;; (set-face-attribute 'font-lock-string-face nil
    ;;                     :foreground "LightYellow1");;"#b0e0e6");;"#708090");;"antique white")
    ;; (set-face-attribute 'font-lock-type-face nil
    ;;                     :foreground "PaleGreen")
    ;; (set-face-attribute 'font-lock-variable-name-face nil
    ;;                     :foreground "burlywood3")
    ;; (set-face-attribute 'font-lock-preprocessor-face nil
    ;;                     :foreground "MediumPurple1")
    ;; (set-face-attribute 'default nil
    ;;                     :foreground "burlywood3")


    (defun post-load-settings ()
      (set-foreground-color "burlywood3")
      (set-background-color "#161616")
      (set-cursor-color "#40FF40")
      (split-window-horizontally))
    (add-hook 'window-setup-hook 'post-load-settings)))

(defvar org-todo-keywords)
(defun add-org-mode ()
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
  )

(defun add-highlight-indentation ()
  (progn
    (require 'highlight-indentation)
    (set-face-background 'highlight-indentation-face "#323232");;"#383838");;"#424242")
    ;; (set-face-background 'highlight-indentation-current-column-face "#")
    ))

(defun add-haskell-mode ()
  "Add haskell major mode."
  (progn
    ;; Haskell-mode installation requirements
    ;; https://github.com/haskell/haskell-mode
    (require 'package)
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)))

(defvar jedi:complete-on-dot)
(defun add-jedi-python-auto-complete ()
  "Configure jedi python auto completion."

  (progn
    ;; Jedi python auto-complete
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t))) ; optional


;; (defun add-cpp-auto-complete ()
;;   "Configure cpp auto completion using 'irony mode' and 'company mode'"
;;   (when (and (package-installed-p 'company)
;;              (package-installed-p 'company-irony))
;;     (progn
;;       ;; company package - http://company-mode.github.io/
;;       ;; Run company in all buffers
;;       (add-hook 'after-init-hook 'global-company-mode)

;;       ;; irony-mode - https://github.com/Sarcasm/irony-mode
;;       ;; C++ completion package setup
;;       (add-hook 'c++-mode-hook 'irony-mode)
;;       (add-hook 'c-mode-hook 'irony-mode)
;;       (add-hook 'objc-mode-hook 'irony-mode)
;;       ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;       ;; irony-mode's buffers by irony-mode's asynchronous function
;;       (defun my-irony-mode-hook ()
;;         (define-key irony-mode-map [remap completion-at-point]
;;           'irony-completion-at-point-async)
;;         (define-key irony-mode-map [remap complete-symbol]
;;           'irony-completion-at-point-async))
;;       (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;       ;; Only needed on Windows
;;       (when (eq system-type 'windows-nt)
;;         (setq w32-pipe-read-delay 0))

;;       ;; company-irony - https://github.com/Sarcasm/company-irony/
;;       ;; Add company-irony to the company backends
;;       (eval-after-load 'company
;;         '(add-to-list 'company-backends 'company-irony)))))


;; (defun remove-irony-from-php-mode ()
;;   "Because php-mode is based on c-mode, irony will be active. We want to
;; de-activate it in any buffer php-mode is active in."

;;   (defun remove-irony-backend ()
;;     (delete 'company-irony 'company-backends))

;;   ;; NOTE(sdsmith): this depends on being added after hook that adds irony
;;   ;; Should be fine because c-mode-hook should be called before php-mode-hook
;;   ;; since php-mode depends on c-mode.
;;   (add-hook 'php-mode-hook 'remove-irony-backend))

(defun add-irony ()
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(defvar desktop-globals-to-save)
(defvar desktop-dirname)
(defun add-desktop ()
  "Configure desktop package."
  (progn
    ;; saves window config
    ;; http://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
    (require 'desktop)
    (desktop-save-mode 1) ; Load desktop at startup

    (defun vars-to-save ()
      (setq history-length 250)
      (add-to-list 'desktop-globals-to-save 'file-name-history))
    (vars-to-save)

    (defun not-to-save ()
      ;; ;; Buffers
      ;; (setq desktop-buffers-not-to-save
      ;;       (concat "")

      ;; ;; Modes
      ;; (add-to-list 'desktop-modes-not-to-save 'some-mode)
      )
    (not-to-save)

    (defun desktop-auto-save ()
      ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
      (if (eq (desktop-owner) (emacs-pid))
          (desktop-save desktop-dirname)))
    (add-hook 'auto-save-hook 'desktop-auto-save)))


(defun add-cedet ()
  ;; NOTE(sdsmith): http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html#sec1

  (load-file "~/.emacs.d/cedet-bzr/trunk/cedet-devel-load.el")

  ;;; Enable CEDET features
  ;; Enables global support for Semanticdb;
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  ;; Enables automatic bookmarking of tags that you edited, so you can return
  ;; to them later with the semantic-mrub-switch-tags command;
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  ;; Activates CEDET's context menu that is bound to right mouse button;
  (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
  ;; Activates highlighting of first line for current tag (function, class,
  ;; etc.);
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  ;; Activates mode when name of current tag will be shown in top line of buffer
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;; Activates use of separate styles for tags decoration (depending on tag's
  ;; class). These styles are defined in the semantic-decoration-styles list;
  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  ;; Activates highlighting of local names that are the same as name of tag
  ;; under cursor;
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-local-symbol-highlight-mode)
  ;; Activates automatic parsing of source code in the idle time;
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  ;; Activates displaying of possible name completions in the idle time.
  ;; Requires that global-semantic-idle-scheduler-mode was enabled;
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-completions-mode)
  ;; Activates displaying of information about current tag in the idle time.
  ;; Requires that global-semantic-idle-scheduler-mode was enabled.
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  ;; NOTE(sdsmith): Following sub-modes are usually useful when you develop
  ;; and/or debug CEDET:
  ;; Shows which elements weren't processed by current parser's rules;
  (add-to-list 'semantic-default-submodes
               'global-semantic-show-unmatched-syntax-mode)
  ;; Shows current parser state in the modeline;
  (add-to-list 'semantic-default-submodes
               'global-semantic-show-parser-state-mode)
  ;; Shows changes in the text that weren't processed by incremental parser yet.
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)

  ;;; System header includes
  ;; NOTE(sdsmith): (semantic-add-system-include "<file path>" '<mode>) to
  ;; include specific paths
  (require 'semantic/bovine/gcc) ; GCC headers



  ;; ;; Setup for java
  ;; (require 'semantic/db-javap)
  ;; (ede-java-root-project "CSC488_Compiler"
  ;;                        :file "~/school/csc488/csc488-team08/RUNCOMPILER.sh"
  ;;                        :srcroot '("src")
  ;;                        :localclasspath '("lib/java-cup-11a-runtime.jar"
  ;;                                          "lib/java-cup-v11a.jar"
  ;;                                          "lib/JFlex.jar")
  ;;                        :classpath '("/home/smiths/school/csc488/csc488-team08/lib/java-cup-11a-runtime.jar"
  ;;                                     "/home/smiths/school/csc488/csc488-team08/lib/java-cup-v11a.jar"
  ;;                                     "/home/smiths/school/csc488/csc488-team08/lib/JFlex.jar"))

  ;; Activate CEDET
  (semantic-mode 1) ; Activate this last
  )



(defun configure-additional-packages ()
  "Load configurations for custom packages."
  (progn
    ;; (add-haskell-mode)
    ;; (add-jedi-python-auto-complete)
    ;;(add-cpp-auto-complete) ; TODO(sdsmith): undo later for cpp completion. Competes with php-mode
    (add-desktop)
    ;;(add-cedet)
    (add-org-mode)
    ;; (add-git-gutter)
    ;; (remove-irony-from-php-mode)
    (add-highlight-indentation)
    ;;(add-irony)
    ))


(defvar my-php-symbol-hash)
(defun company-my-php-backend (command &optional arg &rest ignored)
  (case command
    (prefix (and (eq major-mode 'php-mode)
                 (company-grab-symbol)))
    (sorted t)
    (candidates (all-completions
                 arg
                 (if (and (boundp 'my-php-symbol-hash)
                          my-php-symbol-hash)
                     my-php-symbol-hash

                   (with-temp-buffer
                     (call-process-shell-command
                      "php -r '$all=get_defined_functions();foreach ($all[\"internal\"] as $fun) { echo $fun . \";\";};'"
                      nil t)
                     (goto-char (point-min))
                     (let ((hash (make-hash-table)))
                       (while (re-search-forward "\\([^;]+\\);" (point-max) t)
                         (puthash (match-string 1) t hash))
                       (setq my-php-symbol-hash hash))))))))

(defvar company-backends)
(defun my-php ()
  (add-to-list 'company-backends 'company-my-php-backend))
(add-hook 'php-mode-hook 'my-php)

;; TODO: screws up the config
;; (require 'xterm-color)
;; (setq comint-output-filter-functions
;;       (remove 'ansi-color-process-output comint-output-filter-functions))
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (add-hook 'comint-preoutput-filter-functions
;;                       'xterm-color-filter nil t)))

;; NOTE(stewarts): This is slow. Likely getting applied to the whole buffer.
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region compilation-filter-start (point))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;;
;; Option 2
;; (require 'ansi-color)
;; (defun my-ansi-colorize-buffer ()
;;   (let ((buffer-read-only nil))
;;     (ansi-color-apply-on-region (point-min) (point-max))))
;; (add-hook 'compilation-filter-hook 'my-ansi-colorize-buffer) ;; TODO(stewarts): super slow on huge output
;;
;; Option 3
;; TODO(stewarts): does not apply to compilation window
;; (require 'ansi-color)
;; (defun my-colorize-buffer-window (win)
;;   (ansi-color-apply-on-region (window-start win) (window-end win t)))
;; (defun my-colorize-buffer (win _start)
;;   (mapc #'my-colorize-buffer-window (get-buffer-window-list (window-buffer win) nil 'visible)))
;; (add-hook 'window-scroll-functions 'my-colorize-buffer)
;;
;; Option 4
;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))
(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)
;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))
(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")
(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))
(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))
(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))
(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)
;;
;; Option 5
;; TODO: screws up the whole emacs config
;; (let ((ansi-color-apply-face-function
;;        (lambda (beg end face)
;;          (when face
;;            (put-text-property beg end 'face face)))))
;;   (ansi-color-apply-on-region (point-min) (point-max)))

(defun my-comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))
(define-key comint-mode-map "\C-c\M-o" #'my-comint-clear-buffer)

(defun main ()
  "Main .emacs function"
  (progn
;    (set-package-archives)
    (set-abbrev-table)
    (configure-emacs)
    (configure-syntax)
    (configure-additional-packages)
    ))
(main)

(add-hook 'fundamental-mode-hook 'perforce-cl-desc-setup)
