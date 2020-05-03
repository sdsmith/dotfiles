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

(load-user-file "visuals.el")
(load-user-file "emacs-behaviour.el")
(load-user-file "clipboard-integration.el")
(load-user-file "default-buffers.el")
(load-user-file "frame.el")
(load-user-file "navigation.el")
(load-user-file "utils.el")
(load-user-file "abbrevs.el")

(load-user-file "file-mode-map.el")
(load-user-file "programming-faces.el")                
(load-user-file "programming-visual.el")
(load-user-file "programming-systems.el")
(load-user-file "source-control.el")
(load-user-file "org-mode.el")

;;(load-user-file "instance-persistence.el")
;;(load-user-file "nvidia-mods.el")

(defun configure-syntax ()
  "Configure syntax and highlighting."
  (progn
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
    (configure-syntax)
    ))
(main)
