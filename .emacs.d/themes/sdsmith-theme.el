;;; sdsmith-theme.el --- Custom theme -*- lexical-binding: t -*-

(deftheme sdsmith "My custom dark theme")

(custom-theme-set-faces
 'sdsmith

 ;; Default face
 '(default          ((t (:family "Liberation Mono"
                         :height 100 ; value in 1/10pt => 100 = 10pt
                         :weight normal
                         :width normal
                         :foreground "burlywood3"
                         :background "#282828"))))
 '(cursor           ((t (:background "#40FF40"))))

 ;; Syntax highlighting
 '(font-lock-builtin-face       ((t (:foreground "#DAB98F"))))
 '(font-lock-comment-face       ((t (:foreground "gray60"))))
 '(font-lock-constant-face      ((t (:foreground "DarkKhaki"))))
 '(font-lock-doc-face           ((t (:foreground "burlywood3"))))
 '(font-lock-function-name-face ((t (:foreground "IndianRed1"))))
 '(font-lock-keyword-face       ((t (:foreground "DarkGoldenrod3"))))
 '(font-lock-string-face        ((t (:foreground "#ddddb6"))))
 '(font-lock-type-face          ((t (:foreground "IndianRed1"))))
 '(font-lock-variable-name-face ((t (:foreground "burlywood3"))))
 '(font-lock-preprocessor-face  ((t (:foreground "MediumPurple1"))))

 ;; UI elements
 '(minibuffer-prompt            ((t (:foreground "cyan"))))
 '(highlight                    ((t (:background "#3b3b3b"))))  ; hl-line
 '(region                       ((t (:background "#4a3a6a")))) ; #2a4a7f
 '(isearch-fail                 ((t (:background "red"))))

 ;; terminal colors
 '(term-color-black   ((t (:foreground "#555555" :background "#1c1c1c"))))
 '(term-color-red     ((t (:foreground "#CC4444" :background "#DD6666"))))
 '(term-color-green   ((t (:foreground "#558844" :background "#77AA55"))))
 '(term-color-yellow  ((t (:foreground "#CC8844" :background "#DDB76B"))))
 '(term-color-blue    ((t (:foreground "#5577BB" :background "#6677AA"))))
 '(term-color-magenta ((t (:foreground "#AA55AA" :background "#CC77CC"))))
 '(term-color-cyan    ((t (:foreground "#447788" :background "#66AAAA"))))
 '(term-color-white   ((t (:foreground "#AAAAAA" :background "#DDDDDD")))))

(custom-theme-set-variables
 'sdsmith
 '(cursor-type 'box))

(provide-theme 'sdsmith)
