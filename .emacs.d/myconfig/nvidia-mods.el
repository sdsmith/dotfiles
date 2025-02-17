(defun nvidia-c-setup ()
  ;; Indentation
  (c-set-offset 'inline-open 0)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label 4)

  ;; ;; Add doxygen comments to comment-start-skip
  ;; (setq comment-start-skip "\\(//+\\|//!\\|/\\*+\\)\\s *")
  )
(add-hook 'js-mode-hook 'nvidia-c-setup)
(add-hook 'c-mode-hook 'nvidia-c-setup)
(add-hook 'c++-mode-hook 'nvidia-c-setup)
