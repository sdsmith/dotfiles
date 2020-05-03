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

;; ;; FB: setup proxy to talk to internet
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "fwdproxy:8080")
;;              ("https" . "fwdproxy:8080")))

;; Stop the beeping!!!
(setq visible-bell t)

;; Set this right away so all prompts are readable
(set-face-attribute 'minibuffer-prompt nil :foreground "cyan")

;; Setup customize system
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; NOTE(sdsmith): ALWAYS DO THIS BEFORE ANY PACKAGE CUSTOMIZATION
(load-user-file "package.el")
                
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

(load-user-file "source-control.el")

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

;; line-move-partial is the scrolling lag scurge of the universe. This causes
;; line-move to skip calling line-move-partial. When compiling MODS in a
;; compilation buffer line-move-partial was responsible for _90%_ of the
;; execution time. And it was laggy. Multiple seconds responce laggy. Kill the
;; demon.
;; ref: https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)

;; Vertical indentation guidelines
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-background 'highlight-indent-guides-odd-face "#474747")
(set-face-background 'highlight-indent-guides-even-face "#161616")
(set-face-foreground 'highlight-indent-guides-character-face "gray30")
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(require 'clang-format)
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
    (lambda ()
      (progn
        (when (locate-dominating-file "." ".clang-format")
          (clang-format-buffer))
        ;; Continue to save.
        nil))
    nil
    ;; Buffer local hook.
    t))
(add-hook 'prog-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

(defun setup-completion-engine ()
  ;; Company - code completion
  (require 'company)
  (add-hook 'prog-mode-hook 'company-mode))

(defun setup-helm ()
  ;; Helm - general completion (commands, lists, etc.)
  ;;
  ;; Completion is based on the completion window, not the minibuffer (like emacs
  ;; completion). Helm interactivity happens in the completion window, not the
  ;; minibuffer (like emacs completion). Typing new characters filters conadidates
  ;; in completion window, not minibuffer.
  ;;
  ;; Can navigate to desired value by typing or using `C-n`. Hitting `RET` selects
  ;; currently highlighted item in compeltion window.
  (require 'helm-config)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

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

(defun setup-emacs-behaviour ()
  ;; No more typing the whole yes or no. Just y or n will do.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Set fill column
  (dolist (hook regular-mode-hooks)
    (add-hook hook (lambda () (set-fill-column 80))))
  (add-hook 'fundamental-mode-hook (lambda () (set-fill-column 80)))

  ;; Delete all trailing whitespace
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
  )

(defun setup-frame-style ()
  ;; line numbers
  (require 'linum)
  (global-linum-mode 1)

  ;; simpler column numbers
  (column-number-mode t)

  ;; Display the current function in the mode line
  (dolist (hook regular-mode-hooks)
    (add-hook hook (lambda () (which-function-mode))))
  ;; TODO: Set the font face of face `which-func`
  )

(defun setup-default-buffers ()
  ;; Makes *scratch* empty.
  (setq initial-scratch-message "")

  ;; Removes *Completions* from buffer after you've opened a file.
  (add-hook 'minibuffer-exit-hook
            '(lambda ()
               (let ((buffer "*Completions*"))
                 (and (get-buffer buffer)
                      (kill-buffer buffer)))))

  ;; ;; Removes *messages* from the buffer.
  ;; (setq-default message-log-max nil)
  ;; (kill-buffer "*Messages*")
  ;; tree representation of changes to to walk the undo/redo graph. "C-x u" to open tree for current file.

  ;; Don't show *Buffer list* when opening multiple files at the same time.
  (setq inhibit-startup-buffer-menu t)

  ;; Show only one active window when opening multiple files at the same time.
  (add-hook 'window-setup-hook 'delete-other-windows)
  )

;; go to last edit location
(require 'goto-last-change)
(global-set-key "\C-q" 'goto-last-change)

;; tree representation of changes to walk the undo/redo graph. "C-x u" to open tree for current file.
(require 'undo-tree)
(global-undo-tree-mode)

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

;;; Alternative MacOS clipboard integration
;; Copying/Cutting in console emacs will add it to your mac clipboard
;; Need to also "sudo ym install xclip" along with installing xcip.el
;; Need to also enable X11 Forwarding & trusted X11 Forwarding (ssh -X -Y)
;; (require 'xclip)
;; (turn-on-xclip)

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

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

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
             ("\\.zsh-theme$"     . sh-mode)
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

(defun configure-additional-packages ()
  "Load configurations for custom packages."
  (progn
    ;; (add-haskell-mode)
    ;; (add-jedi-python-auto-complete)
    ;;(add-cpp-auto-complete) ; TODO(sdsmith): undo later for cpp completion. Competes with php-mode
    ;;(add-desktop)
    (add-org-mode)
    ;; (add-git-gutter)
    ;; (remove-irony-from-php-mode)
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

;; (defun my-comint-clear-buffer ()
;;   (interactive)
;;   (let ((comint-buffer-maximum-size 0))
;;     (comint-truncate-buffer)))
;; (define-key comint-mode-map "\C-c\M-o" #'my-comint-clear-buffer)

(defun main ()
  "Main .emacs function"
  (progn
    ;; (set-package-archives)
    (setup-emacs-behaviour)
    (setup-frame-style)
    (setup-default-buffers)
    (set-abbrev-table)
    (configure-emacs)
    (configure-syntax)
    (configure-additional-packages)
    (setup-completion-engine)
    (setup-helm)
    ))
(main)
