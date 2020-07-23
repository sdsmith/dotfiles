
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
                  (clang-format-buffer)))
            ;; Continue to save
            nil
	    ;; Buffer local hook.
	    t))
(dolist (hook '(c++-mode-hook c-mode-hook))
  (add-hook hook 'clang-format-save-hook-for-this-buffer))

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t) ;; Cache project index
(setq projectile-mode-line-prefix " ρ")
(append '(".cquery_cached_index") projectile-globally-ignored-directories)

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
(setq
 lsp-enable-snippet nil ; disable yasnippet integration
 ;; Disable declaration preview overlay on top right of frame
 lsp-ui-sideline-show-code-actions nil
 lsp-ui-sideline-show-hover nil)

(defun setup-cquery ()
  (require 'cquery)
  (setq
   cquery-extra-init-params '(:completion (:detailedLabel t))
   cquery-sem-highlight-method 'font-lock
   company-transformers nil
   company-lsp-async t
   company-lsp-cache-candidates nil
   xref-prompt-for-identifier '(not
                                xref-find-definitions
                                xref-find-definitions-other-window
                                xref-find-definitions-other-frame
                                xref-find-references))
  (defun cquery//enable()
    (condition-case nil
        (lsp)
      (user-error nil)))
  (add-hook 'c-mode-common-hook #'cquery//enable))

;; Setup lsp server
(if (file-exists-p (expand-user-work-file "lsp-server-setup.el"))
    (load-user-work-file "lsp-server-setup.el")
  (setup-cquery))

(require 'company-lsp)
(add-to-list 'company-backends 'company-lsp)

;; TODO(sdsmith): lsp-ui
;; (require 'lsp-ui)
;; (setq lsp-ui-sideline-show-code-actions nil)
;; (setq lsp-ui-sideline-show-hover nil)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

;;; TRAMP
;; run emacs server over TCP for "remote" access
(setq server-port "1492")
(setq server-use-tcp t)
;; fast auto complete in minibuffer
(setq tramp-completion-reread-directory-timeout nil)
;; ignore login prompts w/ proper ssh-config
(setq tramp-default-method "sshx")
;; tramp clobbers ssh ControlPath setting by default to avoid password prompt
(setq tramp-use-ssh-controlmaster-options nil)
(require 'tramp)
(setq tramp-message-show-message nil)
;; Let tramp search $PATH as given to the $USER on the remote machine
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
