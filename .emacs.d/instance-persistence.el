(require 'desktop)
(defvar desktop-globals-to-save)
(defvar desktop-dirname)

;; saves window config
;; http://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
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
(add-hook 'auto-save-hook 'desktop-auto-save))
