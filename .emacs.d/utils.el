;; Stopping the emacs server
(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

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

;; (defun my-comint-clear-buffer ()
;;   (interactive)
;;   (let ((comint-buffer-maximum-size 0))
;;     (comint-truncate-buffer)))
;; (define-key comint-mode-map "\C-c\M-o" #'my-comint-clear-buffer)

(defun my/return-t (orig-fun &rest args)
  "Return true"
  t)

(defun disable-y-or-n-p (orig-fun &rest args)
  "Disable yes or no prompt a sinlge time for the associated function.

Usage: (advise-add 'foo :around #'disable-y-or-n-p)
"
  (advice-add 'yes-or-no-p :around #'my/return-t)
  (advice-add 'y-or-n-p :around #'my/return-t)
  (let ((res (apply orig-fun args)))
    (advice-remove 'yes-or-no-p #'my/return-t)
    (advice-remove 'y-or-n-p #'my/return-t)
    res))
