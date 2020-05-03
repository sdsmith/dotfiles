;; Set this right away so all prompts are readable before the full theme is
;; setup.
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
