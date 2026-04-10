;; Default syntax style
(setq c-default-style "linux")

;; Line numbers
;; NOTE: linum has major perf issues, so prefer built-in display-line-numbers. Available in emacs v26.1. linum deprecated in v29.1.
(if (version< emacs-version "26.1")
    (use-package linum
      :config
      (my/add-to-multiple-hooks '(lambda () (linum-mode 1))
                                '(prog-mode-hook text-mode-hook)))
  (my/add-to-multiple-hooks #'display-line-numbers-mode
                            '(prog-mode-hook text-mode-hook)))

;; Vertical indentation guidelines
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-background 'highlight-indent-guides-odd-face "#474747")
(set-face-background 'highlight-indent-guides-even-face "#161616")
(set-face-foreground 'highlight-indent-guides-character-face "gray30")
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(require 'highlight-doxygen)
(add-hook 'c++-mode-hook 'highlight-doxygen-mode)
(add-hook 'c-mode-hook 'highlight-doxygen-mode)
(add-hook 'js-mode-hook 'highlight-doxygen-mode)
(set-face-attribute 'highlight-doxygen-comment nil :foreground "grey60" :background "grey20")
(set-face-attribute 'highlight-doxygen-variable nil :foreground "grey80")

;; TODO: screws up the config
;; (require 'xterm-color)
;; (setq comint-output-filter-functions
;;       (remove 'ansi-color-process-output comint-output-filter-functions))
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (add-hook 'comint-preoutput-filter-functions
;;                       'xterm-color-filter nil t)))

;; NOTE(stewarts): This is slow. Likely getting applied to the whole buffer.
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region compilation-filter-start (point))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;;
;; Option 2
;; (require 'ansi-color)
;; (defun my-ansi-colorize-buffer ()
;;   (let ((buffer-read-only nil))
;;     (ansi-color-apply-on-region (point-min) (point-max))))
;; (add-hook 'compilation-filter-hook 'my-ansi-colorize-buffer) ;; TODO(stewarts): super slow on huge output
;;
;; Option 3
;; TODO(stewarts): does not apply to compilation window
;; (require 'ansi-color)
;; (defun my-colorize-buffer-window (win)
;;   (ansi-color-apply-on-region (window-start win) (window-end win t)))
;; (defun my-colorize-buffer (win _start)
;;   (mapc #'my-colorize-buffer-window (get-buffer-window-list (window-buffer win) nil 'visible)))
;; (add-hook 'window-scroll-functions 'my-colorize-buffer)
;;
;; Option 4
;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))
(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)
;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))
(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")
(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))
(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))
(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))
(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)
;;
;; Option 5
;; TODO: screws up the whole emacs config
;; (let ((ansi-color-apply-face-function
;;        (lambda (beg end face)
;;          (when face
;;            (put-text-property beg end 'face face)))))
;;   (ansi-color-apply-on-region (point-min) (point-max)))
