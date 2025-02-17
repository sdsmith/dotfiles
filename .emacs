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
(setq custom-file (expand-file-name "myconfig/custom.el" user-emacs-directory))
(load custom-file)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/"))
  "Emacs init directory for current user")

(defconst user-workfiles-init-dir "~/.workdotfiles/.emacs.d/myconfig"
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

(load-user-file "myconfig/utils.el")

;; NOTE(sdsmith): ALWAYS DO THIS BEFORE ANY PACKAGE CUSTOMIZATION
(load-user-file "myconfig/package.el")
(eval-when-compile
  (require 'use-package))

;; Automatically byte compile any elisp files
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(load-user-file "myconfig/visuals.el")
(load-user-file "myconfig/emacs-behaviour.el")
(load-user-file "myconfig/clipboard-integration.el")
(load-user-file "myconfig/default-buffers.el")
(load-user-file "myconfig/frame.el")
(load-user-file "myconfig/navigation.el")
(load-user-file "myconfig/abbrevs.el")

(load-user-file "myconfig/file-mode-map.el")
(load-user-file "myconfig/programming-faces.el")
(load-user-file "myconfig/programming-visuals.el")
(load-user-file "myconfig/programming-systems.el")
(load-user-file "myconfig/source-control.el")
(load-user-file "myconfig/org-mode.el")
(load-user-file "myconfig/shorten-major-mode-names.el")
(load-user-file "myconfig/shell-integration.el")

;;(load-user-file "myconfig/instance-persistence.el")
;;(load-user-file "myconfig/nvidia-mods.el")

;; Utils
(load-user-file "myconfig/external/hierarchy-major-modes.el")
