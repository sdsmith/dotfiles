;; ;;; MacOS clipboard integration
;; ;; ref: http://iancmacdonald.com/macos/emacs/tmux/2017/01/15/macOS-tmux-emacs-copy-past.html
;; ;; NOTE: requires reattach-to-user-namespace
;; ;; (https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard)
;; (defun copy-from-osx ()
;;   "Use OSX clipboard to paste."
;;   (shell-command-to-string "reattach-to-user-namespace pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   "Add kill rinf entries (TEXT) to OSX clipboard. PUSH."
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "reattach-to-user-namespace" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (if (eq system-type 'darwin)
;;     (progn
;;       (setq interprogram-cut-function 'paste-to-osx)
;;       (setq interprogram-paste-function 'copy-from-osx)))



;; Alternative all-OS clipboard integration
;; Copying/Cutting in console emacs will add it to the clipboard
;; Need to also "sudo ym install xclip" along with installing xclip.el
;; Need to also enable X11 Forwarding & trusted X11 Forwarding (ssh -X -Y)
;;
;; IMPORTANT(sdsmith): X11 server advertises itself, but I'm not sure if it
;; broadcasts locally or opens the port to the world.
;; (require 'xclip)
;; (xclip-mode 1)
