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

(load-user-file "emacs-behaviour.el")
(load-user-file "clipboard-integration.el")
(load-user-file "default-buffers.el")
(load-user-file "frame.el")
(load-user-file "navigation.el")
(load-user-file "utils.el")

(load-user-file "programming-visual.el")
(load-user-file "programming-systems.el")
(load-user-file "org-mode.el")
(load-user-file "source-control.el")
;;(load-user-file "instance-persistence.el")

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

(defun main ()
  "Main .emacs function"
  (progn
    ;; (set-package-archives)
    ;;(setup-emacs-behaviour)
    ;;(setup-frame-style)
    ;;(setup-default-buffers)
    (set-abbrev-table)
    (configure-emacs)
    (configure-syntax)
    ))
(main)
