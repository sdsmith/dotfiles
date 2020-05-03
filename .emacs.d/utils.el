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
