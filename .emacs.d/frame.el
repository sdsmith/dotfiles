;; line numbers
(require 'linum)
(global-linum-mode 1)

;; simpler column numbers
(column-number-mode t)

;; Display the current function in the mode line
(dolist (hook regular-mode-hooks)
  (add-hook hook (lambda () (which-function-mode))))
;; TODO: Set the font face of face `which-func`

