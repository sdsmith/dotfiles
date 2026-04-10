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
;;; Package bootstrap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gun.org/package/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Refresh archive contents on first run (new machine setup)
(unless package-archive-contents
  (package-refresh-contonets))

;; Install packages if they are missing
;; NOTE: use-package is built-in from Emacs 29
(require 'use-package)
;; Install package if missing. Skips needing to write `(use-package ... :ensure t ...)`
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal workflow
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
  ;; @perf Don't re-read remote directory contents
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
;; | **Vertico**    | Renders the vertical candidate list in the minibuffer    |
;; | **Orderless**  | Matching: space-separated tokens, any order              |
;; | **Marginalia** | Annotations on candidates (file sizes, signatures,       |
;; |                |   keybindings)                                           |
;; | **Consult**    | Enhanced commands: buffer switcher, grep, line search,   |
;; |                |   with live preview                                      |
;; | **Embark**     | Actions on a candidate while the minibuffer is open      |
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :init (vertico-mode))

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
  ("C-c g" . consult-reipgrep)
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
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

;; Icons and type info in the completion popup
(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; IMPORTANT: Run once to download the lldb-vscode DAP adapter:
  ;; M-x dap-gdb-lldb-setup
  (setq dap-auto-configure-features
	'(sessions locals breakpoints expressions tooltip))
  :bind-keymap ("C-c d" . dap-mode-map)
  :bind (:map dap-mode-map
	      ("d" . dap-debug)
	      ("b" . dap-breakpoint-toggle)
	      ("n" . dap-next)
	      ("i" . dap-step-in)
	      ("o" . dap-step-out)
	      ("c" . dap-continue)
	      ("e" . dap-eval-thing-at-point)))

;; IMPORTANT: put project-specific template in `.dir-locals.el` at the
;; project root and configure it as you would launch.json

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quality of life
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; which-key: shows available key continuations after any prefix
(use-package which-key
  :init (which-key-mode))

;; magit: git interface
(use-package magit
  :bind ("C-c m" . magit-status))

;; hl-todo: highlights TODO/FIXME/NOTE/HACK comments
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

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

;; undo-tree: visualize undo history as a tree (C-x u)
(use-package undo-tree
  :init (global-undo-tree-mode)
  :custom
  ;; Store undo history files outside of source tree
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/artifacts/undo-tree"))))

;; winner-mode: C-c left/right to undo/redo window layout changes
(winner-mode 1)

;; windmove: S-arrow to move between windows
(windmove-default-keybindings)

;; auto-window-vscroll: fight scrolling lag
;; NOTE: without this. line-move-partial causes multi-second lag in large buffers
(setq auto-window-vscroll nil)

;; C-g should quit whatever is happening
(defun my/keyboard-quit-dwim ()
  "Do What I Mean and quit the minibuffer or current command."
  (interactive)
  (if (> (minibuffer-depth) 0)
      (abort-recursive-edit)
    (keyboard-quit)))
(global-set-key (kbd "C-g") #'my/keyboard-quit-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macOS settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: verify macOS settings
(when IS-MAC
  (setq mac-command-modifier 'super
	mac-options-modifier 'meta
	mac-right-option-modifier nil)

  ;; Enable pixel-level smooth scrolling
  (setq mac-mouse-wheel-smooth-scroll t)

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
