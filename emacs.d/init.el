;;; init.el --- Main Emacs initialization
;;; Commentary:
;;; This is my Emacs config.
;;; There are many like it, but this one is mine.
;;; Code:

;;;;;;;;;;;;;;;;;;; Package management ;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("gnu"          . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa"        . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("tromey"       . "http://tromey.com/elpa/") t)

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

;;;;;; General packages

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t))

(use-package flycheck
  :commands global-flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :config
    (setq flycheck-pos-tip-timeout 7
	  flycheck-display-errors-delay 0.5)
    (flycheck-pos-tip-mode 1)))

(use-package yasnippet
  :commands yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package exec-path-from-shell
  :if (memq system-type '(gnu gnu/linux darwin))
  :init
  (customize-set-variable 'exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(use-package smex
  :bind (("M-x" . smex))
  :config
  (setq smex-save-file "~/.emacs.d/smex-items")
  (smex-initialize))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;;;;; Navigation

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-switch-project-action 'projectile-dired))

(use-package dired-x
  :ensure nil
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-subtree
  :defer 1
  :bind (:map dired-mode-map
              ("<right>" . dired-subtree-insert)
              ("<left>" . dired-subtree-remove)))

(use-package ido-completing-read+
  :ensure t
  :config
  (setq ido-auto-merge-work-directories-length -1
        ido-enable-flex-matching t
        ido-use-filename-at-point nil)
  (ido-mode 1)
  (ido-ubiquitous-mode 1))

(use-package dumb-jump
  :ensure t
  :bind (("C-c ." . dumb-jump-go)
         ("C-c d j" . dumb-jump-go)
         ("C-c d o" . dumb-jump-go-other-window)
         ("C-c d i" . dumb-jump-go-prompt)
         ("C-c d x" . dumb-jump-go-prefer-external)
         ("C-c d z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

(use-package recentf
  :defer 1
  :init
  (setq recentf-exclude
        '("/\\.git/.*\\'"
          "/elpa/.*\\'"
          "/cache/.*\\'"
          ".*\\.gz\\'")
        recentf-max-saved-items 50
        recentf-max-menu-items 35
	recentf-auto-cleanup 'never)
  (recentf-mode 1))

;;;;;; Git

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t)
  (add-hook 'magit-post-refresh-hook
	    'git-gutter:update-all-windows))

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
  :config
  (setq js-indent-level 2))

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; https://github.com/mola-T/flymd
  (use-package flymd))

;;;;;; LSP

(use-package lsp-mode
  :hook (prog-mode-hook . lsp-mode)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("flow" "lsp"))
    :major-modes '(flow-mode)
    :server-id 'flow-ls)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ("C-z ." . lsp-ui-peek-find-definitions)
	      ("C-z ?" . lsp-ui-peek-find-references)
	      ("C-z i" . lsp-ui-imenu)
              ("C-z d" . lsp-describe-thing-at-point)
              ("C-z r" . lsp-find-references)
	      ("C-z R" . lsp-rename))
  :config
  (remove-hook 'flymake-diagnostic-functions
               'flymake-proc-legacy-flymake)
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil))

(use-package company-lsp
  :after (company lsp-mode)
  :config
  (add-to-list 'company-backends 'company-lsp)
  :custom
  (company-lsp-async t)
  (company-lsp-enable-snippet t))

;;;;;; Lisp

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)))

;;;;;; Clojure

;; https://github.com/clojure-emacs/clojure-mode
;; https://github.com/clojure-emacs/cider
;; https://github.com/clojure-emacs/clj-refactor.el

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
	 ("\\.cljc\\'" . clojurec-mode)
	 ("\\.cljs\\'" . clojurescript-mode))
  :hook ((clojure-mode . enable-paredit-mode)
	 (clojure-mode . subword-mode)
	 (clojure-mode . rainbow-delimiters-mode))
  :config
  (use-package clojure-mode-extra-font-locking))

(use-package cider
  :after clojure-mode
  :init
  (setq cider-repl-display-help-banner nil
        cider-repl-history-file "~/.emacs.d/cider-history")
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (defun browse-current-ns ()
    (interactive)
    (cider-browse-ns
     (with-current-buffer (current-buffer)
       (cider-current-ns))))
  :bind
  (("C-c C-M-b" . cider-browse-ns-all)
   ("C-c M-b" . browse-current-ns)))

(use-package clj-refactor
  :after clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c r"))

;;;;;; Python

;; https://github.com/jorgenschaefer/elpy
;; sudo pacman -Sy ipython
;; pip install rope flake8 importmagic autopep8 yapf black --user

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
  (add-to-list 'magic-mode-alist '("// @flow" . flow-mode))
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
	      ("M-." . godef-jump)
	      ("M-," . pop-tag-mark)
	      ("C-c C-r" . go-rename))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (use-package go-guru)
  (use-package go-autocomplete)
  (ac-config-default))

(use-package go-rename
  :commands (go-rename))

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
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
  :commands racer-mode
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
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

;;;;;;;;;;;;;;;;;;; Customizations ;;;;;;;;;;;;;;;;;;;

;;;;;; User Interface

(use-package doom-themes
  :init (load-theme 'doom-opera t))

;; Max size window on startup
(toggle-frame-maximized)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t
      inhibit-splash-screen t)

;; Show line numbers
(global-linum-mode)

;; No blinking cursor
(blink-cursor-mode 0)

;; No bell
(setq ring-bell-function 'ignore)

;; Full path in title bar
(setq-default frame-title-format "%b (%f)")

;; Turn off menu bars
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Setup font size
(require 'subr-x)
(if-let (font-height (getenv "EMACS_FONT_HEIGHT"))
    (set-face-attribute
     'default nil :height (string-to-number font-height))
  (set-face-attribute 'default nil :height 120))

;; Use UTF-8
(prefer-coding-system 'utf-8)

;; Unique buffer names dependent on file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;; Editing

;; Disable Ctrl-Z minimization/suspension of emacs
(global-set-key (kbd "C-z") nil)

;; Use hippie-expand for text autocompletion
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Auto indent on new line
(electric-indent-mode 1)

;;;;;; Local files

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Automatic backups
(setq backup-directory-alist` (("." . ,"~/.emacs.d/backups"))
      auto-save-default nil)

;; Write custom's settings to separate file (gitignored)
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
