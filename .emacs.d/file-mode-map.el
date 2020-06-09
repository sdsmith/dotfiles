    ;; Load major modes
(autoload 'glsl-mode "glsl-mode" nil t) ; OpenGL Shader Language
(add-to-list 'load-path  "~/.emacs.d/488-source-lang/")
(when (require '488-lang-mode nil 'noerror)) ; CSC488 Source Language (Winter 2016)
(require 'json-mode)
(require 'glsl-mode)
(require 'asm-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

;; Associate file extentions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"           . c++-mode)
         ("\\.h$"             . c++-mode)
         ("\\.c$"             . c++-mode)
         ("\\.txt$"           . indented-text-mode)
         ("\\.emacs$"         . emacs-lisp-mode)
         ("\\.vert$'"         . glsl-mode)
         ("\\.frag$'"         . glsl-mode)
         ("\\.cmake$"         . cmake-mode)
         ("^CMakeLists.txt"   . cmake-mode)
         ("\\.el$"            . emacs-lisp-mode)
         ("\\.org$"           . org-mode)
         ("\\.org.gpg$"       . org-mode)
         ("\\.ref$"           . org-mode)
         ("\\.ref.gpg$"       . org-mode)
         ("\\.notes$"         . org-mode)
         ("\\.js$"            . js-mode)
         ("\\.json$"          . json-mode)
         ("makedefs\\.inc$"   . makefile-gmake-mode)
         ("makesrc\\.inc$"    . makefile-gmake-mode)
         ("Makefile\\.*$"     . makefile-gmake-mode)
         ("make.+\\.inc$"     . makefile-gmake-mode)
         ("\\.spc$"           . js-mode)
         ("\\.nvasm$"         . asm-mode)
         ("\\.gdb$"           . gdb-script-mode)
         ("\\.bashrc$"        . sh-mode)
         ("\\.bashrc_.+$"     . sh-mode)
         ("\\.zsh-theme$"     . sh-mode)
         ("\\.cs$"            . csharp-mode)
         ) auto-mode-alist))
