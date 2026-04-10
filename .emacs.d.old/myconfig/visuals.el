;; Font size
(defun new-frame-setup (frame)
  (if (display-graphic-p frame)
      ;; graphical display
      (set-face-attribute 'default nil :family "Liberation Mono" :height 90 :weight 'normal :width 'normal)
;;      (set-face-attribute 'default nil :family "Liberation Mono" :height 120 :weight 'normal :width 'normal)
    ;; terminal display
    (set-face-attribute 'default nil :family "Liberation Mono" :height 120 :weight 'normal :width 'normal)))
;; Run for existing frames
(mapc 'new-frame-setup (frame-list))
;; Run when a new frame is created
(add-hook 'after-make-frame-functions 'new-frame-setup)

;; Set color scheme
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray60") ; #999999
(set-face-attribute 'font-lock-constant-face nil :foreground "DarkKhaki") ; #bdb76b
(set-face-attribute 'font-lock-doc-face nil :foreground "burlywood3") ; #cdaa7d
(set-face-attribute 'font-lock-function-name-face nil :foreground "IndianRed1") ; #ff6a6a
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3") ; #cd950c
;;(set-face-attribute 'font-lock-string-face nil :foreground "LightYellow1") ; #ffffe0
(set-face-attribute 'font-lock-string-face nil :foreground "#ddddb6")
(set-face-attribute 'font-lock-type-face nil :foreground "IndianRed1") ; #ff6a6a
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3") ; #cdaa7d
(set-face-attribute 'font-lock-preprocessor-face nil :foreground "MediumPurple1") ; #ab82ff
(set-face-attribute 'default nil :foreground "burlywood3") ; #cdaa7d

(add-to-list 'default-frame-alist '(foreground-color . "burlywood3")) ; #cdaa7d
(add-to-list 'default-frame-alist '(background-color . "#161616"))
(add-to-list 'default-frame-alist '(cursor-color     . "#40FF40"))

(set-face-attribute 'minibuffer-prompt nil :foreground "cyan") ; #00ffff

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'highlight
                     ;; "#232323" ; dark gray
                     ;; "#b22222" ; red pop
                     ;; "#8b1a1a" ; deep red
                     ;; "#8b0000" ; red
                     "#3b3b3b")

(set-face-background 'isearch-fail "red") ; #ff0000
(set-face-background 'region "blue") ; #0000ff
