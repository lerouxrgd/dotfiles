;;; init.el --- Main Emacs initialization

;;; Commentary:

;;; This is my Emacs config.
;;; There are many like it, but this one is mine.
;;; My Emacs config is my best friend.  It is my life.
;;; I must master it as I must master my life.

;;; Code:

;; Startup GC tuning
(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;;;;;;;;;;;;;;;;;;; Package management ;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("gnu"          . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa"        . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("tromey"       . "https://tromey.com/elpa/") t)

;; No auto package loading, that's handled via use-package
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (use-package cl))

(setq use-package-always-ensure t)

(require 'bind-key)

;;;;;; Interface

(use-package doom-themes
  :init
  (load-theme 'doom-opera t)  ; Define theme
  (menu-bar-mode         -1)  ; Turn off menu bars
  (tool-bar-mode         -1)  ; Turn off tool bar
  (scroll-bar-mode       -1)  ; Turn off native OS scroll bars
  (blink-cursor-mode     -1)  ; Turn off blinking cursor
  (global-hl-line-mode    1)  ; Highlight current line
  (show-paren-mode        1)  ; Highlight matching parenthesis
  (global-linum-mode      1)  ; Show line numbers
  (electric-indent-mode   1)  ; Auto indent on new line
  (set-frame-parameter nil 'undecorated t) ; No window decoration
  (toggle-frame-maximized)                 ; Max size window on startup
  (prefer-coding-system 'utf-8)            ; Use UTF-8
  (fset 'yes-or-no-p 'y-or-n-p)            ; Use y/n for questions
  (set-face-attribute                      ; Setup font size
   'default nil
   :height (string-to-number (or (getenv "EMACS_FONT_HEIGHT") "110")))

  (setq-default indent-tabs-mode nil)    ; Don't use hard tabs
  (setq
   inhibit-startup-message      t        ; Go to scratch buffer on startup
   inhibit-splash-screen        t        ; No splash screen
   ring-bell-function           'ignore  ; No bell
   create-lockfiles             nil      ; No need for ~ files when editing
   auto-save-default            nil)     ; No auto-save of file-visiting buffers

  ;; Local files
  (setq
   backup-directory-alist '(("." . "~/.emacs.d/backups"))
   custom-file            "~/.emacs.d/custom.el")
  (when (file-exists-p custom-file)
    (load custom-file))

  (defun toggle-comment-on-line ()
    (interactive)
    (comment-or-uncomment-region
     (line-beginning-position) (line-end-position)))

  (defun show-file-name ()
    (interactive)
    (message (buffer-file-name)))

  (defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted.
They will be reverted though if they were modified outside emacs.
Buffers visiting files no existing/readable will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      (when (and filename (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            (with-demoted-errors "Error: %S"
              (with-current-buffer buf
                (revert-buffer :ignore-auto :noconfirm)))
          (let (kill-buffer-query-functions)
            (kill-buffer buf)
            (message "Killed unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

  :bind
  (("S-C-<left>"  . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>"  . shrink-window)
   ("S-C-<up>"    . enlarge-window)
   ("C-x C-x"     . nil)
   ("C-x C-x C-x" . exchange-point-and-mark)
   ("C-x C-x C-." . show-file-name)
   ("C-x C-x C-r" . revert-all-file-buffers)
   ("C-x C-b"     . ibuffer)
   ("M-/"         . hippie-expand)
   ("C-;"         . toggle-comment-on-line)
   ("C-z"         . nil)))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;;;;;; General packages

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package smex
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file "~/.emacs.d/smex-items")
  (smex-initialize))

(use-package company
  :hook (after-init . global-company-mode)
  :bind (("TAB" . company-indent-or-complete-common)
         :map company-active-map
         ("<right>" . company-abort))
  :config (setq company-tooltip-align-annotations t))

(use-package flycheck
  :hook ((after-init      . global-flycheck-mode)
         (emacs-lisp-mode . flycheck-on-save))
  :init
  (defun flycheck-on-save ()
    (setq flycheck-check-syntax-automatically '(mode-enabled save)))
  :config
  (use-package flycheck-pos-tip
    :config
    (setq flycheck-pos-tip-timeout 7
	  flycheck-display-errors-delay 0.5)
    (flycheck-pos-tip-mode 1)))

(use-package ido-completing-read+
  :config
  (setq ido-auto-merge-work-directories-length -1
        ido-enable-flex-matching t
        ido-use-filename-at-point nil)
  (ido-mode 1)
  (ido-ubiquitous-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-diff-refine-hunk t)
  (use-package magit-ediff
    :ensure nil
    :init
    (setq ediff-force-faces t)
    :config
    (setq magit-ediff-dwim-show-on-hunks t
          ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-x C-x C-s" . yas-insert-snippet))
  :config
  (use-package yasnippet-snippets)
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          yas-hippie-try-expand)))

(use-package iedit
  :custom (iedit-toggle-key-default (kbd "C-:")))

(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

(use-package exec-path-from-shell
  :if (memq system-type '(gnu gnu/linux darwin))
  :config (exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-arguments nil))

(use-package recentf
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 35
	recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package editorconfig
  :config (editorconfig-mode 1))

;;;;;; Navigation

(use-package windmove
  :config (windmove-default-keybindings))

(use-package buffer-move
  :bind (("<M-S-up>"    . buf-move-up)
         ("<M-S-down>"  . buf-move-down)
         ("<M-S-left>"  . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

(use-package nswbuff
  :bind (("C-<tab>"         . nswbuff-switch-to-next-buffer)
         ("<C-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
  :config
  (setq nswbuff-display-intermediate-buffers t
        nswbuff-exclude-buffer-regexps
        '("^ " "^[*]" "null" "^Magit.*"))
  (custom-set-faces
   `(nswbuff-current-buffer-face
     ((t (:weight bold
                  :background ,(doom-color 'blue)
                  :foreground ,(doom-color 'bg)))))
   `(nswbuff-separator-face
     ((t (:foreground ,(doom-color 'magenta)))))
   `(nswbuff-special-buffers-face
     ((t (:foreground ,(doom-color 'yellow)))))))

(use-package treemacs
  :bind (("C-x t" . treemacs-project)
         :map treemacs-mode-map
         ("C-<tab>"         . (lambda () (interactive)))
         ("<C-iso-lefttab>" . (lambda () (interactive))))

  :config
  (defun treemacs-project ()
    (interactive)
    (unless (treemacs-current-workspace)
      (treemacs--find-workspace))
    (treemacs-do-add-project-to-workspace
     (cdr (project-current))
     (car (last (butlast (split-string (cdr (project-current)) "/")))))
    (treemacs-select-window))

  (setq treemacs-persist-file "/dev/null"
        treemacs-collapse-dirs 7))

(use-package dired-subtree
  :defer 1
  :bind (:map dired-mode-map
              ("<right>" . dired-subtree-insert)
              ("<left>"  . dired-subtree-remove)))

(use-package helm
  :bind ("M-X" . 'helm-M-x))

;; sudo pacman -Syu ripgrep
(use-package helm-rg
  :bind (("C-c c" . helm-rg-project)
         ("C-c C" . helm-rg)
         ("C-c f" . helm-find-project)
         ("C-x F" . helm-recentf))

  :config
  (defun helm-rg-project (pattern)
    (interactive (list (helm-rg--get-thing-at-pt)))
    (let ((default-directory (cdr (project-current))))
      (helm-rg pattern t)))
  
  (defun helm-find-project ()
    (interactive)
    (let ((default-directory (cdr (project-current))))
      (helm-find nil)))

  (setq helm-always-two-windows t
        helm-split-window-inside-p t
        helm-rg--color-format-argument-alist
        '((red :cmd-line magenta :text-property magenta)))

  (custom-set-faces
   `(helm-rg-active-arg-face
     ((t (:foreground ,(doom-color 'green)))))
   `(helm-rg-error-message
     ((t (:foreground ,(doom-color 'yellow)))))
   `(helm-rg-line-number-match-face
     ((t (:foreground ,(doom-color 'teal) :underline t))))
   `(helm-rg-file-match-face
     ((t (:foreground ,(doom-color 'teal) :underline t))))
   `(helm-rg-preview-line-highlight
     ((t (:weight bold
                  :background ,(doom-color 'blue)
                  :foreground ,(doom-color 'bg)))))))

;; yay -Syu global
;; sudo pacman -Syu ctags
;; pip install --user pygments
(use-package helm-gtags
  :bind (("C-M-m g" . toggle-helm-gtags-mode)
         :map helm-gtags-mode-map
         ("C-c C-t" . helm-gtags-create-tags)
         ("M-."     . helm-gtags-find-tag-from-here)
         ("M-,"     . helm-gtags-pop-stack))

  :preface
  (defvar helm-gtags-on nil)
  :config
  (defun toggle-helm-gtags-mode ()
    (interactive)
    (cond (helm-gtags-on ; toggle off
           (setq helm-gtags-on nil)
           (remove-hook 'prog-mode-hook 'helm-gtags-mode)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (funcall 'helm-gtags-mode -1))))
          (t ; toggle on
           (setq helm-gtags-on t)
           (add-hook 'prog-mode-hook 'helm-gtags-mode)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (funcall 'helm-gtags-mode 1))))))

  (setq helm-always-two-windows t
        helm-split-window-inside-p t)

  :custom
  (helm-gtags-auto-update t)
  (helm-gtags-path-style 'relative))

(use-package helm-xref
  :config
  (setq helm-always-two-windows t
        helm-split-window-inside-p t
        xref-show-xrefs-function 'helm-xref-show-xrefs)
  (custom-set-faces
   `(helm-xref-file-name
     ((t (:foreground ,(doom-color 'teal)))))))

(use-package dumb-jump
  :bind ("C-c ." . dumb-jump-go)
  :hook (c-mode-common
         . (lambda ()
             (local-set-key (kbd "C-c .") 'dumb-jump-go)))
  :config
  (setq dumb-jump-selector 'helm))

;;;;;; LSP

(use-package lsp-mode
  :hook (prog-mode . lsp-mode)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("flow" "lsp"))
    :major-modes '(flow-mode)
    :server-id 'flow-mode)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-z !" . flymake-show-diagnostics-buffer)
              ("C-z d" . lsp-describe-thing-at-point)
              ("C-z m" . lsp-ui-imenu)
              ("C-z r" . lsp-rename)
              ("C-z ." . lsp-find-definition)
	      ("C-z ?" . lsp-find-references)
              ("C-z I" . lsp-find-implementation)
              ("C-z D" . lsp-find-declaration)
              ("C-z T" . lsp-find-type-definition)
              ("C-z A" . xref-apropos-at-point))
  :config
  (remove-hook 'flymake-diagnostic-functions
               'flymake-proc-legacy-flymake)

  (defun xref-apropos-at-point (x)
    (interactive (list (xref--read-identifier "Find apropos of: ")))
    (xref-find-apropos x))

  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil))

(use-package company-lsp
  :after (company lsp-mode)
  :config
  (add-to-list 'company-backends 'company-lsp)
  :custom
  (company-lsp-async t)
  (company-lsp-enable-snippet t))

;;;;;; Simple formatting

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :mode "\\.yaml\\'")

;; https://github.com/dryman/toml-mode.el
(use-package toml-mode
  :mode "\\.toml\\'")

;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.avsc\\'" . json-mode))
  :config (setq js-indent-level 2))

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; https://github.com/jrblevin/markdown-mode
;; https://github.com/mola-T/flymd
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (use-package flymd
    :config (setq flymd-close-buffer-delete-temp-files t)))

(use-package hi-lock
  :bind (("s-a" . highlight-symbol-at-point)
         ("s-d" . unhighlight-regexp))
  :config (setq hi-lock-face-defaults '("hi-pink"))
  :custom-face (hi-pink ((t (:background "pink4")))))

;;;;;; Lisp

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode       . enable-paredit-mode)))

(use-package rainbow-delimiters)

;;;;;; Clojure

;; https://github.com/clojure-emacs/clojure-mode
;; https://github.com/clojure-emacs/cider
;; https://github.com/clojure-emacs/clj-refactor.el

(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
	 ("\\.cljc\\'" . clojurec-mode)
	 ("\\.cljs\\'" . clojurescript-mode))
  :hook ((clojure-mode . enable-paredit-mode)
	 (clojure-mode . subword-mode)
	 (clojure-mode . rainbow-delimiters-mode))
  :config
  (use-package clojure-mode-extra-font-locking))

(use-package cider
  :after clojure-mode
  :bind (("C-c C-M-b" . cider-browse-ns-all)
         ("C-c M-b"   . browse-current-ns))
  :hook ((cider-mode      . eldoc-mode)
         (cider-repl-mode . paredit-mode))
  :init
  (defun browse-current-ns ()
    (interactive)
    (cider-browse-ns
     (with-current-buffer (current-buffer)
       (cider-current-ns))))
  (setq cider-repl-display-help-banner nil
        cider-repl-history-file "~/.emacs.d/cider-history"))

(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c r"))

;;;;;; Java

;; https://github.com/emacs-lsp/lsp-java

(use-package lsp-java
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package lsp-java-treemacs
  :load-path "lsp-java-treemacs.el"
  :commands lsp-java-treemacs-register)

;;;;;; Python

;; https://github.com/jorgenschaefer/elpy

;; sudo pacman -Syu ipython
;; pip install --user rope flake8 importmagic autopep8 yapf black

(use-package elpy
  :hook (python-mode . elpy-enable)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  (elpy-mode))

;;;;;; Javascript

;; https://github.com/codesuki/add-node-modules-path

(use-package add-node-modules-path
  :preface
  (define-derived-mode flow-mode js-mode "flow-mode"
    "A dedicated major flow-mode, useful for LSP setup")
  (add-to-list 'magic-mode-alist '("// @flow"        . flow-mode))
  (add-to-list 'magic-mode-alist '("/\\* @flow \\*/" . flow-mode))
  :after flycheck
  :hook (js-mode . add-node-modules-path)
  :config
  (flycheck-add-mode 'javascript-eslint 'flow-mode))

;;;;;; Go

;; https://github.com/dominikh/go-mode.el
;; https://github.com/rogpeppe/godef
;; https://github.com/nsf/gocode#emacs-setup
;; https://github.com/syohex/emacs-go-eldoc

;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef/...
;; go get -u github.com/nsf/gocode

(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
	      ("M-."     . godef-jump)
	      ("M-,"     . pop-tag-mark)
	      ("C-c C-r" . go-rename))
  :hook (before-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports")
  (use-package go-guru)
  (use-package go-rename)
  (use-package go-autocomplete)
  (ac-config-default))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

;;;;;; Rust

;; https://github.com/rust-lang/rust-mode
;; https://github.com/flycheck/flycheck-rust
;; https://github.com/racer-rust/emacs-racer
;; https://github.com/kwrooijen/cargo.el

;; rustup component add rust-src rust-analysis
;; rustup component add rustfmt rls
;; rustup toolchain add nightly
;; cargo +nightly install racer

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :after flycheck
    :hook (flycheck-mode . flycheck-rust-setup)))

(use-package racer
  :hook
  ((rust-mode . racer-mode)
   (rust-mode . eldoc-mode))
  :bind (:map rust-mode-map
	      ("M-." . racer-find-definition))
  :config
  (use-package company-racer
    :config
    (add-to-list 'company-backends 'company-racer)
    (setq company-tooltip-align-annotations t)))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;;; init.el ends here
