;; Custom text (For windowed mode)
(add-to-list 'default-frame-alist '(font . "Liberation Mono-9.5"))
;; (set-face-attribute 'default t :font "Liberation Mono-10") ;; BUG(sdsmith): causes 'emacs --daemon' to error during startup
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray60")
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "DarkKhaki")
(set-face-attribute 'font-lock-doc-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "IndianRed1")
(set-face-attribute 'font-lock-keyword-face nil
                    :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil
                    :foreground "LightYellow1")
(set-face-attribute 'font-lock-type-face nil
                    :foreground "IndianRed1")
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground "burlywood3")
(set-face-attribute 'font-lock-preprocessor-face nil
                    :foreground "MediumPurple1")
(set-face-attribute 'default nil
                    :foreground "burlywood3")

;; Original config
;;
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-9.5"))
;; ;; (set-face-attribute 'default t :font "Liberation Mono-10") ;; BUG(sdsmith): causes 'emacs --daemon' to error during startup
;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
;; (set-face-attribute 'font-lock-comment-face nil :foreground "gray60");;"dark grey");;"thistle4" ;;"gray50")
;; (set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
;; (set-face-attribute 'font-lock-doc-face nil :foreground "burlywood3")
;; (set-face-attribute 'font-lock-function-name-face nil
;;                                     ; :foreground "DarkGoldenrod3")
;;                                     ; :foreground "IndianRed1")
;; (set-face-attribute 'font-lock-keyword-face nil
;;                     :foreground "DarkGoldenrod3")
;; (set-face-attribute 'font-lock-string-face nil
;;                     :foreground "LightYellow1");;"#b0e0e6");;"#708090");;"antique white")
;; (set-face-attribute 'font-lock-type-face nil
;;                     :foreground "PaleGreen")
;; (set-face-attribute 'font-lock-variable-name-face nil
;;                     :foreground "burlywood3")
;; (set-face-attribute 'font-lock-preprocessor-face nil
;;                     :foreground "MediumPurple1")
;; (set-face-attribute 'default nil
;;                     :foreground "burlywood3")
