;; Show file path in status bar when using non-unqiue names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Show full file path as window title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f"
                               (dired-directory dired-directory "%b"))))

;; Display current column number in buffer status bar
(setq column-number-mode t)

;; simpler column numbers
(column-number-mode t)

;; Display the current function in the mode line
(add-hook 'prog-mode-hook 'which-function-mode)
;; TODO: Set the font face of face `which-func`

