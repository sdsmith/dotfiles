;;; .emacs Configuration file -*- no-byte-compile -*-
;;
;; NOTE: Don't byte compile this file since "load-prefer-newer" won't be set
;; until this file is loaded.
;;
;; TODO(sdsmith):
;; - insert biolerplate into new files
;; - don't use setq! https://emacs.stackexchange.com/questions/17386/display-all-setq-possibilities

;; Tips
;; - describe-face: Describes font face. Defaults to face at point.
;; - ibuffer: nice table of open buffers

;; Load the newest file, not just the byte compiled file
(setq load-prefer-newer t)

;; There will be a package-initialize call Emacs! Stop adding the call to my
;; file!
(setq package--init-file-ensured t)

;; Setup customize system
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/"))
  "Emacs init directory for current user")

(defconst user-workfiles-init-dir "~/.workdotfiles/.emacs.d/"
  "Emacs work files init directory for current user")

(defun expand-user-file (file)
  (expand-file-name file user-init-dir))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-user-file file)))

(defun expand-user-work-file (file)
  (expand-file-name file user-workfiles-init-dir))

(defun load-user-work-file (file)
  (interactive "f")
  "Load a file in the current user's work files configuration directory"
  (load-file (expand-user-work-file file)))

(load-user-file "utils.el")

;; NOTE(sdsmith): ALWAYS DO THIS BEFORE ANY PACKAGE CUSTOMIZATION
(load-user-file "package.el")

;; Automatically byte compile any elisp files
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(load-user-file "visuals.el")
(load-user-file "emacs-behaviour.el")
(load-user-file "clipboard-integration.el")
(load-user-file "default-buffers.el")
(load-user-file "frame.el")
(load-user-file "navigation.el")
(load-user-file "abbrevs.el")

(load-user-file "file-mode-map.el")
(load-user-file "programming-faces.el")
(load-user-file "programming-visuals.el")
(load-user-file "programming-systems.el")
(load-user-file "source-control.el")
(load-user-file "org-mode.el")
(load-user-file "shorten-major-mode-names.el")

;;(load-user-file "instance-persistence.el")
;;(load-user-file "nvidia-mods.el")

(server-start)
(start-process-shell-command
 "scp" "*scp*" "scp -p ~/.emacs.d/server/server devf:~/.emacs.d/remoteserver/server")
