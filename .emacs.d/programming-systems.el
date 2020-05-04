;; Company - code completion
(require 'company)
(add-hook 'prog-mode-hook 'company-mode)

(defvar my-php-symbol-hash)
(defun company-my-php-backend (command &optional arg &rest ignored)
  (case command
	(prefix (and (eq major-mode 'php-mode)
		     (company-grab-symbol)))
	(sorted t)
	(candidates (all-completions
		     arg
		     (if (and (boundp 'my-php-symbol-hash)
			      my-php-symbol-hash)
			 my-php-symbol-hash

		       (with-temp-buffer
			 (call-process-shell-command
			  "php -r '$all=get_defined_functions();foreach ($all[\"internal\"] as $fun) { echo $fun . \";\";};'"
			  nil t)
			 (goto-char (point-min))
			 (let ((hash (make-hash-table)))
			   (while (re-search-forward "\\([^;]+\\);" (point-max) t)
			     (puthash (match-string 1) t hash))
			   (setq my-php-symbol-hash hash))))))))

(defvar company-backends)
(defun my-php ()
  (add-to-list 'company-backends 'company-my-php-backend))
(add-hook 'php-mode-hook 'my-php)

(defun add-haskell-mode ()
  "Add haskell major mode."
  (progn
    ;; Haskell-mode installation requirements
    ;; https://github.com/haskell/haskell-mode
    (require 'package)
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)))
(add-haskell-mode)

;; (defvar jedi:complete-on-dot)
;; (defun add-jedi-python-auto-complete ()
;;   "Configure jedi python auto completion."

;;   (progn
;;     ;; Jedi python auto-complete
;;     (add-hook 'python-mode-hook 'jedi:setup)
;;     (setq jedi:complete-on-dot t))) ; optional

(require 'clang-format)
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
	    (lambda ()
	      (progn
		(when (locate-dominating-file "." ".clang-format")
		  (clang-format-buffer))
		;; Continue to save.
		nil))
	    nil
	    ;; Buffer local hook.
	    t))
(add-hook 'prog-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
