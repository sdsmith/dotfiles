;; Additional Highlighting
(make-face 'font-lock-comment-user-face)
(make-face 'font-lock-comment-todo-face)
(make-face 'font-lock-comment-note-face)
(make-face 'font-lock-comment-important-face)
(make-face 'font-lock-comment-study-face)
(make-face 'font-lock-comment-readme-face)
(make-face 'font-lock-comment-bug-face)
(make-face 'font-lock-comment-debug-face)
(make-face 'font-lock-comment-doc-face)
(make-face 'font-lock-comment-war-face)
(make-face 'font-lock-comment-bug-ref-face)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(
           ("\\<\\(TODO(\\w+?):\\)" 1 'font-lock-comment-todo-face t)
           ("\\<\\(NOTE(\\w+?):\\)" 1 'font-lock-comment-note-face t)
           ("\\<\\(IMPORTANT(\\w+?):\\)" 1 'font-lock-comment-important-face t)
           ("\\<\\(STUDY(\\w+?):\\)" 1 'font-lock-comment-study-face t)
           ("\\<\\(README(\\w+?):\\)" 1 'font-lock-comment-readme-face t)
           ("\\<\\(BUG(\\w+?):\\)" 1 'font-lock-comment-bug-face t)
           ("\\<\\(DEBUG(\\w+?):\\)" 1 'font-lock-comment-debug-face t)
           ("\\<\\(DOC(\\w+?):\\)" 1 'font-lock-comment-doc-face t)
           ("\\<\\(WAR(\\w+?):\\)" 1 'font-lock-comment-war-face t)
           ("\\(TODO\\|NOTE\\|IMPORTANT\\|STUDY\\|README\\|BUG\\|DOC\\)(\\(\\w+?\\)):"
            2 'font-lock-comment-user-face t)
           ("WAR(\\(\\w+?\\)):" 1 'font-lock-comment-bug-ref-face t)
           )))
      regular-modes)

(modify-face 'font-lock-comment-user-face "thistle4" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-todo-face "Red3" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-note-face "DarkOliveGreen" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-important-face "gold" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-study-face "gold" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-readme-face "DodgerBlue" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-bug-face "chartreuse" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-debug-face "chartreuse" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-doc-face "DeepPink2" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-war-face "Red3" nil nil t nil nil nil nil)
(modify-face 'font-lock-comment-bug-ref-face "chartreuse" nil nil t nil nil nil nil)

;; Add keywords to cpp
(font-lock-add-keywords 'c++-mode
                        '(("constexpr" . font-lock-keyword-face))
                        '(("nullptr"   . font-lock-keyword-face)))
