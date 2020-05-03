;; line-move-partial is the scrolling lag scurge of the universe. This causes
;; line-move to skip calling line-move-partial. When compiling MODS in a
;; compilation buffer line-move-partial was responsible for _90%_ of the
;; execution time. And it was laggy. Multiple seconds responce laggy. Kill the
;; demon.
;; ref: https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)

;; Change window configuration (C-c Left|Right to navigate history)
(when (fboundp 'winner-mode)
  (winner-mode 1))
  ;; (global-set-key (kbd "M-<up>") 'enlarge-window)
  ;; (global-set-key (kbd "M-<down>") 'shrink-window)
  ;; (global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
  ;; (global-set-key (kbd "M-<right>") 'enlarge-window-horizontally))

;; go to last edit location
(require 'goto-last-change)
(global-set-key "\C-q" 'goto-last-change)

;; tree representation of changes to walk the undo/redo graph. "C-x u" to open tree for current file.
(require 'undo-tree)
(global-undo-tree-mode)
