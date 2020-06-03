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
              (when (locate-dominating-file "." ".clang-format")
                (if (and buffer-file-name (string= (file-name-extension buffer-file-name) "thrift"))
                    nil
                  (clang-format-buffer))))

            ;; Continue to save
            nil
	    ;; Buffer local hook.
	    t))
(add-hook 'prog-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t) ;; Cache project index
(setq projectile-mode-line-prefix " ρ")

(require 'helm-projectile)
(helm-projectile-on)

;; TODO(stewarts): not sure how to activate this...
(require 'helm-company)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; cquery
;; ref: https://github.com/cquery-project/cquery/wiki/Emacs#configure
;;
;; Tips:
;;   M-.  Synonymous with 'jump to definition'
(setq lsp-keymap-prefix "C-c i") ; Must be before require
(require 'lsp)
(define-key lsp-mode-map (kbd "C-c i") lsp-command-map)
(setq lsp-enable-snipper nil) ; disable yasnippet integration

(require 'cquery)
(setq cquery-executable "/usr/local/bin/cquery")
(defun cquery//enable()
  (condition-case nil
      (lsp)
    (user-error nil)))
(add-hook 'c-mode-hook #'cquery//enable)
(add-hook 'c++-mode-hook #'cquery//enable)
