;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; IMPORTANT: Run once to download the lldb-vscode DAP adapter:
;;   M-x dap-gdb-lldb-setup

;; Helpful commands and flows
;;
;; Bookmark remote projects:
;; C-x r m -> <bookmark_name>
;; C-x b -> <bookmark_name> -> Vertico finds it
;;
;; Shell:
;; C-` toggle persistent vterm per projectile project
;; Remote file editing:
;; - C-x C-f /ssh:user@hostname:/path/to/file
;; - via TRAMP. Will automatically start clangd on the remote machine and connect to lsp.
;; - needs clangd on the remote system (Debian/Ubuntu: clangd, RHEL/Fedora: clang-tools-extra, macOS: xcode-select --install)
;;
;; Remote shell:
;; - ssh via vterm
;; - for persistent remote session, on remote machine `tmux attach || tmux new`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move it to a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Platform detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'sdsmith t)

;; set display-line-numbers-mode faces after packages have loaded. something is overriding them
(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'line-number nil :background "#000000" :foreground "gray40")
            (set-face-attribute 'line-number-current-line nil :background "#000000" :foreground "gray60" :weight 'bold)))


;; highlight current line
;;
;; NOTE: can have issues in vterm buffers, so only use in programming or text
;; modes
(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package bootstrap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu
.org/package/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Refresh archive contents on first run (new machine setup)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages if they are missing
;; NOTE: use-package is built-in from Emacs 29
(require 'use-package)
;; Install package if missing. Skips needing to write `(use-package ... :ensure t ...)`
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terminal workflow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vterm: full terminal emulator inside emacs
;;
;; Requires libvterm system package

(use-package vterm
  :custom
  (vterm-max-scrollback 100000)
  (vterm-shell "/bin/zsh")
  (vterm-kill-buffer-on-exit t) ; close buf when shell exists
  (vterm-timer-delay 0.01) ; low latency for interactive use
  (vterm-keymap-exceptions
   '("C-`" "C-<f1>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "M-:"))
  :config
  ;; C-` toggles vterm open/closed
  (define-key vterm-mode-map (kbd "C-`") #'vterm-toggle)
  (define-key vterm-mode-map (kbd "C-<f1>") #'vterm-toggle)
  ;; cycle between multiple vterm buffers
  (define-key vterm-mode-map (kbd "s-n") #'vterm-toggle-forward)
  (define-key vterm-mode-map (kbd "s-p") #'vterm-toggle-backward))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vterm-toggle: Quake-style toggle between code an terminal
;;
;; Key opens project-scoped vterm split, toggle to close
;; Project scope persistent shell (cd, env, history preserved) based on Projectile project root
(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-scope 'project) ; shell per project root
  ;;(vterm-toggle-scope 'dedicated) ; one global shell
  (vterm-toggle-fullscreen-p nil) ; open as split, not fullscreen
  ;; vterm-toggle-hide-method options:
  ;; - delete-window: delete vterm window
  ;; - nil: toggle without closing vterm window, just jump back and forth between vterm and code buffer
  ;; - reset-window-configration (yes, misspelled): reset window configuration
  (vterm-toggle-hide-method 'delete-window)
  :bind
  ("C-`" . vterm-toggle)
  ("C-<f1>" . vterm-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile: run build command with error-location parsing
;;
;; M-x compile
;; Remembers lat command; C-u M-x compile to change it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package compile
  :custom
  (compilation-scroll-output 'first-error) ; scroll to first error
  (compilation-ask-about-save nil) ; save without asking
  :bind
  ("C-c b" . compile)
  ("C-c B" . recompile)) ; rerun last compile

;; Support colors in compilation
(use-package fancy-compilation
  :config (fancy-compilation-mode))

;; wrap lines in compilation buffers
(add-hook 'compilation-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAMP: remote file editing
;; open as /ssh:user@host:/path/to/file
;; NOTE: lsp backend (ie clangd) should run on the remote machine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: verify and check best practices
(use-package tramp
  :custom
  ;; SSH uses existing ~/.ssh/config, including ControlMaster multiplexing for fast reconnects
  (tramp-default-method "ssh")
  ;; Don't re-read remote directory contents
  (tramp-completion-reread-directory-timeout nil)
  ;; Preserve SSH ControlMaster setting from ssh config. Reuse connections so each file open doesn't re-handshake
  (tramp-ssh-controlmaster-options
   (concat "-o ControlMaster=auto "
	   "-o ControlPath='~/.ssh/cm-%%r@%%h:%%p' "
	   "-o ControlPath=10m"))
  ;; Cache remote file attrs
  (remote-file-name-inhibit-cache nil)
  (tramp-verbose 1) ; set to 5 to debug connection issues
  :config
  ;; Don't let projectile index remote filesystems. It will try to walk the full remote tree
  (defadvice projectile-project-root (around ignore-remote activate)
    (unless (file-remote-p default-directory) ad-do-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minibuffer narrowing / dropdowns
;;
;; | Package        | Job                                                      |
;; | -------------- | ---------------------------------------------------------|
;; | Vertico        | Renders the vertical candidate list in the minibuffer    |
;; | Orderless      | Matching: space-separated tokens, any order              |
;; | Marginalia     | Annotations on candidates (file sizes, signatures,       |
;; |                |   keybindings)                                           |
;; | Consult        | Enhanced commands: buffer switcher, grep, line search,   |
;; |                |   with live preview                                      |
;; | Embark         | Actions on a candidate while the minibuffer is open      |
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :init (vertico-mode))

;; Set sort behaviour based on command
(use-package vertico-multiform
  :after vertico
  :ensure nil ; ships with vertico
  :init (vertico-multiform-mode)
  :custom
  (vertico-multiform-commands
   '((find-file (vertico-sort-function . vertico-sort-alpha)))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind
  ("C-x b" . consult-buffer)
  ("C-s" . consult-line)
  ("C-c g" . consult-ripgrep)
  ("M-g g" . consult-goto-line)
  ("M-g i" . consult-imenu)
  :custom
  (consult-project-function #'projectile-project-root))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In-buffer completion popup
;;
;; Autocomplete dropdown that appears as you type code. Reads from
;; completion-at-point-functions, which lsp-mods populates with
;; clangd's (or other backend's completions).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: `(lsp-completion-provider :none)` causes lsp-mode to not
;; activate company-mode, routing completions through
;; `completion-at-point` which Corfu picks up automatically.
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  (corfu-on-exact-match nil) ; don't auto-accept on single candidate lists
  (corfu-preselect 'prompt)  ; cursor stays at prompt, don't preselect first candidate
  (corfu-cycle t)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
              ("C-g" . corfu-quit)
              ("<escape>" . corfu-quit)))

;; Icons and type info in the completion popup
(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package yasnippet
  :config
  (yas-global-mode 1))
;; Community snippets
(use-package yasnippet-snippets
  :after yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default)
  (projectile-project-search-path '("~/src" "~/projects"))
  ;; Don't index remote filesystems. TRAMP defadvice handles this, but extra safety
  (projectile-enable-caching t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language server
;;
;; | Action             | Key       |
;; | ------------------ | --------- |
;; | Go to definition   | `M-.`     |
;; | Go back            | `M-,`     |
;; | Find references    | `s-l g r` |
;; | Rename symbol      | `s-l r r` |
;; | Code actions       | `s-l a a` |
;; | Format buffer      | `s-l = =` |
;; | Toggle inlay hints | `s-l T h` |
;; | Show documentation | `s-l h h` |
;; | Header <-> Impl    | `C-c o`   |
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . lsp-deferred)
  :custom
  ;; @perf
  (lsp-idle-delay 0.3)
  (lsp-log-io nil)
  (lsp-completion-provider :none) ; Corfu handles UI, not company-mode

  ;; clangd arguments
  (lsp-clients-clangd-args
   '("--background-index" ; persit index to .cache/clangd/
     "--clang-tidy" ; run clang-tidy checks
     "--completion-style=detailed" ; full signatures in completion
     "--header-insertion=iwyu" ; include-what-you-use style
     "--pch-storage=memory" ; faster for projects using PCH
     "-j=4")) ; @perf background index threads

  ;; UI
  (lsp-headerline-breadcrumb-enable t)
  (lsp-inlay-hints-enable t) ; inline paramter names and types

  :config
  (lsp-enable-which-key-integration t)

  ;; source/header toggle - clangd-aware. better than ff-find-other-file
  :bind (:map lsp-mode-map
	      ("C-c o" . lsp-clangd-find-other-file)))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-peek-enable t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diagnostics
;;
;; | Action            | Key                            |
;; | ----------------- | ------------------------------ |
;; | C-c ! l           | list all diagnostics in buffer |
;; | C-c ! n / C-c ! p | jump to next/previous error    |
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugger
;;
;; Implements Debuger Adapter Protocol (DAP) using lldb for C++ debugging.
;;
;; | Action            | Key                       |
;; | ----------------- | ------------------------- |
;; | Start session     | `C-c d d` → pick template |
;; | Toggle breakpoint | `C-c d b`                 |
;; | Continue          | `C-c d c`                 |
;; | Step over         | `C-c d n`                 |
;; | Step into         | `C-c d i`                 |
;; | Step out          | `C-c d o`                 |
;; | Eval at point     | `C-c d e`                 |
;;
;; IMPORTANT: Requires setup file for debugging like launch.json:
;;
;; Pattern 1: Global templates in init.el
;; For generic or personal projects, you register named templates in your config:
;; elisp(dap-register-debug-template
;;   "My Project Debug"
;;   (list :type "lldb-vscode"
;;         :program "${workspaceFolder}/build/my_binary"
;;         ...))
;; Then M-x dap-debug shows a list of all registered templates and you pick one. The template is reusable across sessions.
;;
;; Pattern 2: Per-project in .dir-locals.el
;; For team projects where the debug config should live with the code, put it in .dir-locals.el at the project root:
;; elisp;; .dir-locals.el
;; ((nil . ((eval . (dap-register-debug-template
;;                    "myproject"
;;                    (list :type "lldb-vscode"
;;                          :program "${workspaceFolder}/build/myproject"
;;                          :args ["--config" "debug.cfg"]
;;                          :cwd "${workspaceFolder}"))))))
;; Emacs loads .dir-locals.el automatically when you open any file in that directory tree, so the template registers itself. This is the closest equivalent to committing a launch.json to the repo.
;; ${workspaceFolder} resolves to the lsp-mode workspace root — which is your project root where compile_commands.json lives, so it's effectively the same as (projectile-project-root).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTANT: Run once to download the lldb-vscode DAP adapter:
;; M-x dap-gdb-lldb-setup
;; TODO: DAP, when activeted, is capturing all the input from the :bind keys so you can't type them...
;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (require 'dap-lldb)
;;   (require 'dap-gdb-lldb)
;;   (setq dap-auto-configure-features
;; 	    '(sessions locals breakpoints expressions tooltip))
;;   (dap-auto-configure-mode)
;;   :bind-keymap ("C-c d" . dap-mode-map)
;;   :bind (:map dap-mode-map
;; 	      ("d" . dap-debug)
;; 	      ("b" . dap-breakpoint-toggle)
;; 	      ("n" . dap-next)
;; 	      ("i" . dap-step-in)
;; 	      ("o" . dap-step-out)
;; 	      ("c" . dap-continue)
;; 	      ("e" . dap-eval-thing-at-point)))

;; IMPORTANT: put project-specific template in `.dir-locals.el` at the
;; project root and configure it as you would launch.json

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perf tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable bidirectional text scanning for perf
;;
;; Bidirection text scanning is used for right-to-left languages, which I don't
;; read. Just do left to right.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Skip fontification during input
;;
;; Emacs does syntax highlighting (fontification) while typing/changing
;; which can cause micro stutters. Scrolling should be faster.
(setq redisplay-skip-fontification-on-input t)

;; Increase process output buffer for LSP
;;
;; Default is 64KB, low. Bumping this reduces the amount of read calls
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

;; Don't render cursors in non-focused windows
;;
;; NOTE: minimal perf impact for renderer, more a visual preference that happens
;; to be faster
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; savehist: save ring and command history between shutdowns
(use-package savehist
  :init (savehist-mode 1)
  :custom
  (savehist-file "~/.emacs.d/artifacts/savehist")
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 60) ; save every 60 seconds
  ;; Extra variables to persist beyond minibuffer history
  (savehist-additional-variables
   '(search-ring
     regexp-search-ring
     kill-ring
     projectile-project-command-history)))
;; Remove text properties (fonts, overlays, ...) that bloat savehist file before saving
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (cl-remove-if-not #'stringp kill-ring)))))

;; Remember recently edited files
;;
;; M-x recentf-open-files
(use-package recentf
  :init (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-save-file "~/.emacs.d/artifacts/recentf")
  ;; file exclusion list
  (recentf-exclude
   '("~/.emacs.d/elpa/"
     "~/.emacs.d/artifacts/"
     "/tmp/"
     "/ssh:")))

;; save-place-mode: reopen files to the last place visited
(save-place-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quality of life
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; which-key: shows available key continuations after any prefix
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-enable-extended-define-key t))

;; magit: git interface
(use-package magit
  :bind ("C-c v" . magit-status))

;; Git diff indicator in gutter
(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode)
  ;; update after magit operations
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  ;; show diff in margin instead of fringe
  ;; (diff-hl-side 'left)
  (diff-hl-show-staged-changes nil) ; only show unstaged changes
  :config
  ;; highlight on-the-fly when editing
  (diff-hl-flydiff-mode 1)
  ;; show diff in margin when running in the terminal (no fringe available)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

;; hl-todo: highlights TODO/FIXME/NOTE/HACK comments
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))
(setq hl-todo-keyword-faces
      '(("TODO"      . "#CC8844")
        ("FIXME"     . "#CC6644")
        ("BUG"       . "#CC4444")
        ("XXX"       . "#CC4444")
        ("NOTE"      . "#88AA55")
        ("IMPORTANT" . "#DDB76B")
        ("STUDY"     . "#8888CC")
        ("README"    . "#8888CC")
        ("DEBUG"     . "#FFB347")
        ("DOC"       . "#7EC8E3")
        ("WAR"       . "#AA55AA")
        ("HACK"      . "#AA55AA")
        ("@perf"     . "#DDB76B")))

;; Highlight custom annotation keywords
(defun sdsmith/add-comment-annotations ()
  (font-lock-add-keywords
   nil
   '(("\\(@perf\\|@safety\\)"
      0 '(:foreground "#7EC8E3" :weight bold) t))
   'append))
(add-hook 'prog-mode-hook #'sdsmith/add-comment-annotations)

;; clang-format: format on save only if .clang-format exists
(use-package clang-format
  :hook (c++-mode . (lambda ()
		      (add-hook 'before-save-hook
				(lambda ()
				  (when (locate-dominating-file "." ".clang-format")
				    (clang-format-buffer)))
				nil t))))

;; goto-last-change: jump back to where you last edited
(use-package goto-last-change
  :bind ("C-q" . goto-last-change))

;; Undo-tree is unmaintained, use Vundo
(use-package vundo
  :diminish
  :bind* (("C-c _" . vundo))
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;; winner-mode: C-c left/right to undo/redo window layout changes
(winner-mode 1)
;; Reversible C-x 1 (delete other windows)
(defun toggle-delete-other-windows ()
  "Delete other windows in frame, if any, or restor previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)

;; windmove: S-arrow to move between windows
(windmove-default-keybindings)

;; auto-window-vscroll: fight scrolling lag
;; NOTE: without this. line-move-partial causes multi-second lag in large buffers
(setq auto-window-vscroll nil)

;; C-g should quit whatever is happening
(defun sdsmith/keyboard-quit-dwim ()
  "Do What I Mean and quit the minibuffer or current command."
  (interactive)
  (if (> (minibuffer-depth) 0)
      (abort-recursive-edit)
    (keyboard-quit)))
(global-set-key (kbd "C-g") #'sdsmith/keyboard-quit-dwim)

;; Make scratch buffer empty
(setq initial-scratch-message "")

;; Show full file path as window title
(setq frame-title-format
      (list (format "%s %%S: %%j" (system-name))
	    '(buffer-file-name "%f"
			       (dired-directory dired-directory "%b"))))


;; Set undo limit very high
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; TODO: change artifact paths to reference emacs home

;; Move backup file (*.~) to separate directory
(when (not (file-directory-p "~/.emacs.d/artifacts/backups"))
  (make-directory "~/.emacs.d/artifacts/backups"))
(setq backup-directory-alist '((".*" . "~/.emacs.d/artifacts/backups/"))
      backup-by-copying t ; don't delink hardlinks
      version-control t ; use version numbers of backups
      delete-old-versions t ; auto delete excess backups
      kept-new-versions 20 ; how many new version to keep
      kept-old-versions 5) ; how many old to keep

;; Move autosave files (#*#) to separate directory
(when (not (file-directory-p "~/.emacs.d/artifacts/autosaves"))
  (make-directory "~/.emacs.d/artifacts/autosaves"))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/artifacts/autosaves/" t)))

;; 80 column limit for programming
(add-hook 'prog-mode-hook (lambda () (set-fill-column 80)))

;; Try to abide by project style settings by looking for .editorconfig
(use-package editorconfig
  :config (editorconfig-mode 1))
;; For identation, if no .editorconfig, sniff the file to determine tabs vs spaces
(use-package dtrt-indent
  :config (dtrt-indent-global-mode 1))
;; Default to spaces and 4 indent
(setq-default
 indent-tabs-mode nil ; spaces by default
 tab-width 4)

;; Delete all trailing whitespace on changed lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TODO: pkg helpful?

;; Column numbers in mode line display
(column-number-mode 1)

;; Line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Better n/p/f/b navigation
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Multiple cursors
;;
;; NOTE: see .emacs.d/.mc-lists.el for preferences on running commands once per cursor or not
(use-package multiple-cursors
  :bind
  ;; Add cursor above/below
  ("C-S-<up>" . mc/mark-previous-line-this)
  ("C-S-<down>" . mc/mark-next-like-this)
  ;; Mark all occurrences of word/region
  ("C-c m a"    . mc/mark-all-like-this)
  ("C-c m d"    . mc/mark-all-dwim)
  ;; Add cursor at each line in region
  ("C-c m l"    . mc/edit-lines))

;; highlight-indent-guides: show indentation
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "#474747")
  (set-face-background 'highlight-indent-guides-even-face "#161616")
  (set-face-foreground 'highlight-indent-guides-character-face "gray30"))
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Auto-chmod scripts on save if file starts with #!
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Sane re-builder syntax (M-x re-builder)
;;
;; Avoids need to double escape. Changes syntax from `\\(...\\)` to `\(...\)`.
(setq reb-re-syntax 'string)

;; Prevent ffap from pining hostnames
;;
;; find-file-at-point will try to ping anything at the point that looks like a
;; hostname (ie something.com). This causes a multi-second hang if it's
;; unreachable.
(setq ffap-machine-p-known 'reject)

;; Proportional window resizing
;;
;; Rebalances windows when doing splits
(setq window-combination-resize t)

;; Faster mark popping for navigation
;;
;; Every time you jump somewhere, last location is added to the mark ring. Go to
;; last location with C-u C-SPC. This allows you to keep pressing C-SPC to
;; continue popping.
(setq set-mark-command-repeat-pop t)

;; TODO: save-place pkg
;; Recenter after save-place restores position
;;
;;
(advice-add 'save-place-file-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))

;; Revert buffers if file on disc changes
;;
;; Monitors open files on disc and reverts them if they've changed on disc
;; outside of emacs.
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; helpful for dired buffers

;; Repeat mode
;;
;; Allows you to repeat the last key of the last command to repeat that
;; command. Supports a lot of common commands out of the box:
;; C-x o                cycling windows
;; C-x { / C-x }        shrink/grow window horizontally
;; C-x ^                grow window vertically
;; C-x u                undo
;; C-x <left> / C-x <right>     cycle through buf history
;; M-g n / M-g p        jump through next error results
;;
;; Example, undo x 3:
;; - before: C-x u C-x u C-x u
;; - after: C-x u u u
;;
;; NOTE: Can define your own repeat-maps to support additional or custom
;; commands.
;;
;; TODO: should I not do undo-tree now? this makes undo much better
(repeat-mode 1)
(setq repeat-exit-timeout 5) ; exit after 5s of inactivity

;; Remove emacs GUI elements
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (pixel-scroll-mode)) ; scroll pixel-by-pixel

;; Increase garbage collection threshold
(setq gc-cons-threshold 100000000) ; 10MB

;; Tweak Emacs defaults
(setq
 ;; Inhbit startup screen/messages
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 ;; don't put double space after period
 sentence-end-double-space nil
 ;; Stop beeping and flashing
 visible-bell 1
 ;; Use y/n for yes and no
 use-short-answers t
 ;; Disable dialog boxes from GUI Emacs
 use-dialog-box nil
 ;; Save existing clipboard to kill ring before overwriting it.
 ;;
 ;; Example scenario: copy URL from browser, switch to Emacs, kill a line with
 ;; C-k, then try to yank URL with C-y. It would normally be gone and replaced by
 ;; the kill. With this change, C-y is the kill, M-y gets yu back to the URL.
 save-interprogram-paste-before-kill t
 ;; No dups in kill ring
 kill-do-not-save-duplicates t
 ;; C-k delete whole line
 kill-whole-line t
 ;; More scroll perf
 fast-but-imprecise-scrolling t
 ;; Prefer newer elisp files
 load-prefer-newer t
 ;; Quit without precess termination confirmation, if process is running (ex vterm)
 confirm-kill-processes nil
 ;; Auto-select help windows when opened
 help-window-select 1
 ;; Only show relevant command in command buffer list
 read-extended-command-predicate  #'command-completion-default-include-p
 ;; Keep point in the same place while scrolling
 scroll-preserve-screen-position t
 ;; More info in completions
 completions-detailed t
 ;; Highlight error messages more aggressively
 next-error-message-highlight t
 ;; Keep window formatting the same when using minibuffer
 read-minibuffer-restore-windows t)

;; UTF-8 as default encoding
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;; Delete/replace selected text if we start typing
(delete-selection-mode t)

;; De-clutter modeline with diminish.el
(use-package diminish
  :config
  (diminish 'visual-line-mode))

;; mood-line: better looking modeline
(defun sdsmith/project-relative-file-name (include-prefix)
  "Return the project-relative filename, or the full path if INCLUDE_PREFIX is t."
  (letrec
      ((fullname (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
       (root (project-root (project-current)))
       (relname (if fullname (file-relative-name fullname root) fullname))
       (should-strip (and root (not include-prefix))))
    (if should-strip relname fullname)))


(defun sdsmith/mood-line-segment-project-advice (oldfun)
  "Advice to use project-relative file names where possible."
  (let
      ((project-relative (ignore-errors (sdsmith/project-relative-file-name nil))))
    (if
        (and (project-current) (not (and (boundp 'org-src-mode) org-src-mode)) project-relative)
        (propertize (format "%s  " project-relative) 'face 'mood-line-buffer-name)
      (funcall oldfun))))

(use-package mood-line
  :config
  (advice-add 'mood-line-segment-buffer-name :around #'sdsmith/mood-line-segment-project-advice)
  (mood-line-mode))

;; Highlight bracket pairs
(use-package rainbow-delimiters
  :disabled
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; Highlight URLs
(global-goto-address-mode)

;; Use tree-sitter for improved syntax highlighting
(use-package tree-sitter
  :config (global-tree-sitter-mode))
(use-package tree-sitter-langs)

;; Open init.el file shortcut
(defun open-init-file ()
  "Open init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(bind-key "C-c E" #'open-init-file)

;; TODO: kill-buffer should just kill the current buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macOS settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: verify macOS settings
;;
;; IMPORTANT: Requires the mac build of emacs and only works for GUI. Terminal
;; relies on the terminal emulator to provide the expected key combos.
(when IS-MAC
  (setq mac-command-modifier 'super
	mac-options-modifier 'meta
	mac-right-option-modifier nil)

  ;; Enable pixel-level smooth scrolling
  (setq pixel-scroll-precision-mode t)

  ;; Use system trach instead of permanent delete
  (setq delete-by-moving-to-trash t
	trash-directory "~/.Trash")

  ;; Fix PATH.
  ;;
  ;; NOTE: macOS GUI apps don't inherit shell PATH. Ensure clangd,
  ;; cmake, etc are found when emacs is launched from launchd instead
  ;; of terminal.
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Linux settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when IS-LINUX
  (setq x-super-keysym 'super))
(put 'upcase-region 'disabled nil)
