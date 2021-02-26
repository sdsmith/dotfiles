;; Cygwin support
;;

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin64 exists. Assumes that C:\cygwin64\bin is
;; not already in your Windows Path (it generally should not be).
;;
(let* ((cygwin-root "c:/cygwin64")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))

    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

    ;; Allow Emacs to understand cygwin file paths
    (add-to-list 'load-path "~/.emacs.d/external")
    (require 'cygwin-mount)
    (cygwin-mount-activate)

    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))

    ;; NT-emacs assumes a Windows shell. Change to zsh.
    (setq shell-file-name "zsh")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)

    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))
