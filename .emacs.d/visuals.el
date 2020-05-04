;; Set color scheme
(add-to-list 'default-frame-alist '(font . "Liberation Mono-9.5"))
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray60")
(set-face-attribute 'font-lock-constant-face nil :foreground "DarkKhaki")
(set-face-attribute 'font-lock-doc-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-function-name-face nil :foreground "IndianRed1")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "LightYellow1")
(set-face-attribute 'font-lock-type-face nil :foreground "IndianRed1")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-preprocessor-face nil :foreground "MediumPurple1")
(set-face-attribute 'default nil :foreground "burlywood3")

(add-to-list 'default-frame-alist '(foreground-color . "burlywood3"))
(add-to-list 'default-frame-alist '(background-color . "#161616"))
(add-to-list 'default-frame-alist '(cursor-color     . "#40FF40"))

(set-face-attribute 'minibuffer-prompt nil :foreground "cyan")

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'highlight
                     ;; "#232323" ; dark gray
                     ;; "#b22222" ; red pop
                     ;; "#8b1a1a" ; deep red
                     "#8b0000")

(set-face-background 'isearch-fail "red")
(set-face-background 'region "blue")
