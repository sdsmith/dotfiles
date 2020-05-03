;; Show file path in status bar when using non-unqiue names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Show full file path as window title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f"
                               (dired-directory dired-directory "%b"))))

;; line numbers
(require 'linum)
(global-linum-mode 1)

;; Display current column number in buffer status bar
(setq column-number-mode t)

;; simpler column numbers
(column-number-mode t)

;; Display the current function in the mode line
(dolist (hook regular-mode-hooks)
  (add-hook hook (lambda () (which-function-mode))))
;; TODO: Set the font face of face `which-func`

