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
(require 'subr-x)

;;;;;; General packages

(use-package smex)
(use-package ido-completing-read+)

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
    (flycheck-pos-tip-mode +1)))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package yasnippet
  :commands yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

;;;;;; Navigation

(use-package projectile
  :defer 1
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t)
  (setq projectile-switch-project-action 'projectile-dired))

(use-package dired-x
  :ensure nil
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("<right>" . dired-subtree-insert)
             ("<left>" . dired-subtree-remove)))

;;;;;; Git

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t)
  (add-hook 'magit-post-refresh-hook
	    'git-gutter:update-all-windows))

;;;;;; Clojure

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
	 ("\\.cljc\\'" . clojurec-mode)
	 ("\\.cljs\\'" . clojurescript-mode))
  :hook ((clojure-mode . enable-paredit-mode)
	 (clojure-mode . subword-mode)
	 (clojure-mode . rainbow-delimiters-mode))
  :config
  (use-package clojure-mode-extra-font-locking)
  (setq cider-repl-display-help-banner nil)
  (use-package cider
    :init
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'paredit-mode)
    :bind
    (("C-c C-M-b" . cider-browse-ns-all)
     ("C-c M-b" .
      (lambda ()
        (interactive)
        (cider-browse-ns
         (with-current-buffer
             (current-buffer) (cider-current-ns))))))
    :config
    (setq cider-show-error-buffer t)
    (setq cider-auto-select-error-buffer t)
    (setq cider-repl-history-file "~/.emacs.d/cider-history"))
  (use-package clj-refactor
    :init
    (add-hook 'clojure-mode-hook 'clj-refactor-mode)
    :config
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c r")))

;;;;;; Rust

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

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)
	 ("/Pipfile\\'" . toml-mode)))

;;;;;;;;;;;;;;;;;;; Customizations ;;;;;;;;;;;;;;;;;;;

;; Write custom's settings to separate file (gitignored)
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/customizations")
(load "shell-integration.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
;;(load "elisp-editing.el")

;;; init.el ends here
