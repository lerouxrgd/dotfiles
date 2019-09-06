;;; init.el --- Main Emacs initialization -*- lexical-binding: t -*-

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

;; Main frame setup
(setq frame-resize-pixelwise t)
(set-frame-parameter nil 'fullscreen 'maximized)
(set-frame-parameter nil 'undecorated t)
(scroll-bar-mode -1) ; Turn off native OS scroll bars
(tool-bar-mode   -1) ; Turn off tool bar
(menu-bar-mode   -1) ; Turn off menu bars

;;;;;;;;;;;;;;;;;;; Package management ;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("tromey"       . "https://tromey.com/elpa/")))

;; No auto package loading, that's handled via use-package
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (use-package cl))

(setq use-package-always-ensure t)

(use-package gnu-elpa-keyring-update)

(use-package quelpa-use-package
  :config
  (setq quelpa-update-melpa-p nil
        quelpa-checkout-melpa-p nil))

;;;;;;;;;;;;;;;;;;;;; Custom functions ;;;;;;;;;;;;;;;;;;;

(defun unkillable-scratch-buffer ()
  "Disallow killing of scratch and delete its content instead."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (delete-region (point-min) (point-max))
             nil)
    t))

(defun project-or-root ()
  "If git project, find root, otherwise find where Emacs was started."
  (or (cdr (project-current))
      (with-current-buffer "*scratch*" default-directory)))

(defun toggle-comment-on-line ()
  "Toggle comment on line and keep cursor on the toggled line."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position) (line-end-position)))

(defun backward-whitespace (arg)
  "Move to the beginning of the current sequence of whitespaces.
Delegate call to 'forward-whitespace with negative ARG."
  (interactive "^p")
  (forward-whitespace (- arg)))

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted.
They will be reverted though if they were modified outside Emacs.
Buffers visiting files not existing/readable will be killed."
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

(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;; Interface ;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :init
  (load-theme 'doom-nova t) ; Define theme

  (blink-cursor-mode   -1) ; Turn off blinking cursor
  (show-paren-mode      1) ; Highlight matching parenthesis
  (global-hl-line-mode  1) ; Highlight current line
  (column-number-mode   1) ; Show column number
  (electric-indent-mode 1) ; Auto indent on new line

  (prefer-coding-system 'utf-8)         ; Use UTF-8
  (fset 'yes-or-no-p 'y-or-n-p)         ; Use y/n for questions
  (put 'upcase-region   'disabled nil)  ; Allow upcase selection
  (put 'downcase-region 'disabled nil)  ; Allow downcase selection

  (setq
   inhibit-startup-message t         ; Go to scratch buffer on startup
   inhibit-splash-screen   t         ; No splash screen
   ring-bell-function      'ignore)  ; No bell

  (setq-default
   indent-tabs-mode nil ; Don't use hard tabs
   tab-width        4)  ; Sane tab-width

  ;; Setup scrolling
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 100000
        mouse-wheel-scroll-amount '(1 ((shift) . 1)))

  ;; Setup font
  (add-to-list
   'default-frame-alist
   `(font . ,(concat "DejaVu Sans Mono"
                     (let ((size (getenv "EMACS_FONT_SIZE")))
                       (if size (concat "-" size) "")))))

  ;; Setup local files
  (setq
   backup-directory-alist '(("." . "~/.emacs.d/backups"))
   custom-file            "~/.emacs.d/custom.el"
   create-lockfiles       nil   ; No need for ~ files when editing
   auto-save-default      nil)  ; No auto-save of file-visiting buffers
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Customize theme
  (custom-theme-set-faces
   'doom-nova
   `(hl-line ((t (:background ,(doom-color 'bg-alt))))))

  :bind
  (("S-C-<left>"  . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>"  . shrink-window)
   ("S-C-<up>"    . enlarge-window)
   ("C-x C-x"     . nil)
   ("C-x C-x C-x" . exchange-point-and-mark)
   ("C-x C-x C-r" . revert-all-file-buffers)
   ("C-x C-b"     . ibuffer)
   ("C-x C-z"     . repeat)
   ("M-F"         . forward-whitespace)
   ("M-B"         . backward-whitespace)
   ("C-;"         . toggle-comment-on-line)
   ("C-z"         . nil)))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (defun doom-buffer-name ()
    (let ((doom-modeline-buffer-file-name-style 'truncate-with-project))
      (doom-modeline-buffer-file-name)))

  (setq frame-title-format '((:eval (doom-buffer-name)) " - %F")
        doom-modeline-icon t
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-icon nil)

  (custom-set-faces
   `(mode-line-inactive ((t (:background ,(doom-color 'bg-alt)))))))

;;;;;;;;;;;;;;;;;;;; General packages ;;;;;;;;;;;;;;;;;;;;

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
  :config
  (add-hook 'buffer-list-update-hook
            (lambda () (auto-complete-mode -1)))
  (setq company-tooltip-align-annotations t))

(use-package flycheck
  :hook ((after-init      . global-flycheck-mode)
         (emacs-lisp-mode . flycheck-on-save))
  :bind (("C-x !" . flycheck-errors-buffer)
         :map flycheck-error-list-mode-map
         ("<C-return>" . flycheck-goto-error-kill-buffer))

  :init
  (defun flycheck-on-save ()
    (setq flycheck-check-syntax-automatically '(mode-enabled save)))

  (defun flycheck-errors-buffer ()
    (interactive)
    (flycheck-list-errors)
    (select-window (get-buffer-window "*Flycheck errors*")))

  (defun flycheck-goto-error-kill-buffer ()
    (interactive)
    (flycheck-error-list-goto-error)
    (kill-buffer "*Flycheck errors*"))

  :config
  (setq flycheck-display-errors-function
        'flycheck-display-error-messages-unless-error-list)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.20))))

(use-package flycheck-package)

(use-package ido-completing-read+
  :config
  (setq ido-auto-merge-work-directories-length -1
        ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-diff-refine-hunk t)
  (use-package magit-ediff
    :ensure nil
    :config (setq magit-ediff-dwim-show-on-hunks t)))

(use-package git-timemachine
  :bind ("C-x G" . git-timemachine))

(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-force-faces t))

(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

(use-package exec-path-from-shell
  :if (memq system-type '(gnu gnu/linux darwin))
  :config (exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-arguments nil))

(use-package bash-completion
  :config (bash-completion-setup))

(use-package recentf
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 35
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package vlf
  :config (require 'vlf-setup))

(use-package editorconfig
  :config (editorconfig-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;

(use-package iedit
  :bind (("C-:" . iedit-mode)
         :map iedit-mode-keymap
         ("C-h"   . iedit-show/hide-unmatched-lines)
         ("C-M-:" . iedit-switch-to-mc-mode)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package selected
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("l" . downcase-region)
              ("w" . copy-to-register)
              ("m" . apply-macro-to-region-lines))
  :init (selected-global-mode))

(use-package multiple-cursors
  :after selected
  :bind
  (("C-S-<mouse-1>" . mc/add-cursor-on-click)
   :map selected-keymap
   ("SPC" . mc/edit-lines)
   (":"   . mc/mark-all-like-this)
   (">"   . mc/mark-next-like-this)
   ("<"   . mc/mark-previous-like-this)
   ("."   . mc/unmark-next-like-this)
   (","   . mc/unmark-previous-like-this)
   ("C->" . mc/skip-to-next-like-this)
   ("C-<" . mc/skip-to-previous-like-this)
   ("h"   . mc-hide-unmatched-lines-mode)
   :map
   mc/keymap
   ("<backtab>" . mc/vertical-align-with-space))

  :hook (multiple-cursors-mode . mc-selected-keys)
  :config
  (defun mc-selected-keys ()
    (if (bound-and-true-p multiple-cursors-mode)
        (progn (define-key selected-keymap (kbd "}") 'mc/cycle-forward)
               (define-key selected-keymap (kbd "{") 'mc/cycle-backward))
      (progn (define-key selected-keymap (kbd "}") nil)
             (define-key selected-keymap (kbd "{") nil)))))

(use-package visual-regexp
  :bind (("C-x C-x C-?" . vr/query-replace)
         ("C-x C-x C-k" . vr/mc-mark)
         ("C-M-S"       . vr/isearch-forward)
         ("C-M-R"       . vr/isearch-backward))
  :config (use-package visual-regexp-steroids))

(use-package string-inflection
  :bind (("C-x M-i c" . string-inflection-lower-camelcase)
         ("C-x M-i C" . string-inflection-camelcase)
         ("C-x M-i k" . string-inflection-kebab-case)
         ("C-x M-i s" . string-inflection-underscore)
         ("C-x M-i u" . string-inflection-upcase)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind (("M-/" . hippie-expand)
         :map yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB"   . nil))
  :config
  (use-package yasnippet-snippets)
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          yas-hippie-try-expand)))

(use-package auto-yasnippet
  :bind (("C-x C-x C-s" . aya-create)
         ("M-Y"         . aya-expand)))

(use-package undo-tree
  :bind ("C-M-/" . undo-tree-visualize)
  :config (global-undo-tree-mode))

(use-package powerthesaurus
  :bind ("C-x M-s" . powerthesaurus-lookup-word-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;; Navigation ;;;;;;;;;;;;;;;;;;;;

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
     ((t (:foreground ,(doom-color 'orange)))))
   `(nswbuff-special-buffers-face
     ((t (:foreground ,(doom-color 'yellow)))))))

(use-package eyebrowse
  :config
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (eyebrowse-mode t))

(use-package treemacs
  :after doom-themes
  :bind (("C-x t" . treemacs-project)
         :map treemacs-mode-map
         ("C-<tab>"         . (lambda () (interactive)))
         ("<C-iso-lefttab>" . (lambda () (interactive))))

  :config
  (defun treemacs--restore ())
  (defun treemacs--persist ())
  (defun treemacs-project ()
    (interactive)
    (unless (treemacs-current-workspace)
      (treemacs--find-workspace))
    (treemacs-do-add-project-to-workspace
     (project-or-root)
     (car (last (butlast (split-string (project-or-root) "/")))))
    (treemacs-select-window))

  (doom-themes-treemacs-config)
  (setq treemacs-collapse-dirs 7
        treemacs-file-follow-delay 0
        treemacs--workspaces (list (make-treemacs-workspace))))

(use-package dired-subtree
  :defer 1
  :bind (:map dired-mode-map
              ("<right>" . dired-subtree-insert)
              ("<left>"  . dired-subtree-remove)))

(use-package helm
  :preface (require 'helm-config)
  :bind (("C-x x" . helm-command-prefix)
         :map helm-command-map
         ("SPC" . helm-all-mark-rings)
         ("x"   . helm-register)
         ("m"   . helm-imenu)
         ("."   . helm-etags-select)
         ("/"   . helm-find-project))

  :config
  (helm-autoresize-mode t)

  (require 'helm-ring)
  (defun multi-pop-to-mark (orig-fun &rest args)
    (let ((p (point)))
      (dotimes (_ 10)
        (when (= p (point))
          (apply orig-fun args)))))
  (advice-add 'pop-to-mark-command :around #'multi-pop-to-mark)

  (require 'helm-find)
  (defun find-with-gitignore (orig-fun &rest args)
    (let ((find-command (apply orig-fun args)))
      (concat
       find-command
       "-and -exec sh -c "
       "'for f;do git check-ignore -q \"$f\" || printf \"$f\n\"; done' "
       "find-sh {} + "
       "2> /dev/null")))
  (advice-add 'helm-find--build-cmd-line :around 'find-with-gitignore)

  (defun helm-find-project (arg)
    (interactive "P")
    (if arg
        (helm-find arg)
      (let ((default-directory (project-or-root)))
        (helm-find nil)))))

(use-package helm-fd
  :quelpa (helm-fd :fetcher github :repo "lerouxrgd/helm-fd")
  :bind (:map helm-command-map
              ("/" . helm-fd-project)))

(use-package helm-c-yasnippet
  :after yasnippet
  :bind (:map helm-command-map
              ("s" . helm-yas-complete)))

(use-package swiper-helm
  :bind (:map isearch-mode-map
              ("TAB" . swiper-helm-from-isearch))
  :config
  (setq swiper-helm-display-function 'helm-default-display-buffer))

;; sudo pacman -Syu ripgrep
(use-package helm-rg
  :bind (("C-c c" . helm-rg-project)
         ("C-c C" . helm-rg))

  :config
  (defun helm-rg-project (pattern)
    (interactive (list (helm-rg--get-thing-at-pt)))
    (let ((default-directory (project-or-root)))
      (helm-rg pattern t)))

  (setq helm-always-two-windows t
        helm-split-window-inside-p t
        helm-rg--color-format-argument-alist
        '((red :cmd-line green :text-property green)))

  (custom-set-faces
   `(helm-rg-title-face
     ((t (:foreground ,(doom-color 'black) :background ,(doom-color 'base6)))))
   `(helm-rg-active-arg-face
     ((t (:foreground ,(doom-color 'green)))))
   `(helm-rg-error-message
     ((t (:foreground ,(doom-color 'yellow)))))
   `(helm-rg-line-number-match-face
     ((t (:foreground ,(doom-color 'base6)))))
   `(helm-rg-file-match-face
     ((t (:foreground ,(doom-color 'teal)))))
   `(helm-rg-preview-line-highlight
     ((t (:foreground ,(doom-color 'bg) :background ,(doom-color 'highlight)))))
   `(helm-rg-directory-header-face
     ((t (:foreground ,(doom-color 'black) :background ,(doom-color 'base6)))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;; LSP ;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :hook (prog-mode . lsp-mode)
  :config
  (setq lsp-prefer-flymake nil
        lsp-document-highlight-delay 0.1
        lsp-enable-symbol-highlighting nil)

  (add-to-list 'lsp-language-id-configuration '(flow-mode . "flow"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("flow" "lsp"))
    :major-modes '(flow-mode)
    :server-id 'lsp-flow)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-z !" . lsp-ui-flycheck-list)
              ("C-z z" . lsp-toggle-highlighting)
              ("C-z d" . lsp-describe-thing-at-point)
              ("C-z m" . lsp-ui-imenu)
              ("C-z r" . lsp-rename)
              ("C-z ." . lsp-find-definition)
              ("C-z ?" . lsp-find-references)
              ("C-z I" . lsp-find-implementation)
              ("C-z D" . lsp-find-declaration)
              ("C-z T" . lsp-find-type-definition))

  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil)

  (defun lsp-toggle-highlighting ()
    (interactive)
    (if lsp-enable-symbol-highlighting
        (setq lsp-enable-symbol-highlighting nil)
      (setq lsp-enable-symbol-highlighting t)))

  (use-package helm-lsp
    :bind (:map lsp-ui-mode-map
                ("C-z a" . lsp-workspace-symbol)
                ("C-z A" . lsp-global-workspace-symbol))
    :config
    (defun lsp-workspace-symbol ()
      (interactive)
      (helm-lsp-workspace-symbol t))
    (defun lsp-global-workspace-symbol ()
      (interactive)
      (helm-lsp-global-workspace-symbol t))))

(use-package company-lsp
  :after (company lsp-mode)
  :config (add-to-list 'company-backends 'company-lsp))

;;;;;;;;;;;;;;;;;;;;;;; Simple formatting ;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.avsc\\'" . json-mode))
  :config (setq js-indent-level 2))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package mustache-mode)

;; sudo pacman -Syu marked
;; pip install --user grip
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (use-package grip-mode
    :bind (:map markdown-mode-command-map
                ("g" . grip-mode)))
  (setq markdown-command "marked"
        markdown-live-preview-delete-export 'delete-on-export))

(use-package symbol-overlay
  :bind (("C-x C-x C-a" . symbol-overlay-put)
         ("C-x C-x M-a" . symbol-overlay-remove-all)
         ("C-z C-z"     . symbol-overlay-mode))
  :config (setq symbol-overlay-idle-time 0.2))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  (use-package company-terraform
    :config (company-terraform-init)))

(use-package kubernetes
  :commands (kubernetes-overview))

;; npm install -g yaml-language-server
(use-package lsp-yaml
  :quelpa (lsp-yaml :fetcher github :repo "iquiw/lsp-yaml")
  :hook (yaml-mode
         . (lambda ()
             (setq-local lsp-eldoc-render-all t)
             (eldoc-mode)
             (local-set-key
              (kbd "<backtab>") 'company-complete)))
  :config
  (setq lsp-yaml-schemas '(:kubernetes "*.yml")))

(use-package restclient
  :after (helm company)
  :config
  (use-package restclient-helm)
  (use-package company-restclient
    :bind (:map restclient-mode-map
                ("C-c TAB" . company-complete))
    :init (add-to-list 'company-backends 'company-restclient)))

;;;;;;;;;;;;;;;;;;;;;;; Scientific ;;;;;;;;;;;;;;;;;;;;;;;

;; sudo pacman -Syu texlive-core
(use-package latex-preview-pane
  :config (setq pdf-latex-command "xelatex"))

;; sudo pacman -Syu tk gcc-fortran
;; install.packages("lintr")
(use-package ess
  :mode (("\\.[rR]\\'" . ess-r-mode)
         ("\\.jl\\'"   . julia-mode))
  :hook (ess-mode
         . (lambda ()
             (local-set-key
              (kbd "TAB") 'company-indent-or-complete-common)))
  :config
  (setq ess-use-flymake nil))

;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters)

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode       . enable-paredit-mode)))

;; yay -Syu chez-scheme
(use-package geiser
  :preface (setq geiser-active-implementations '(chez))
  :hook ((scheme-mode      . enable-paredit-mode)
         (geiser-repl-mode . enable-paredit-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;; Clojure ;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/clojure-emacs/clojure-mode
;; https://github.com/clojure-emacs/cider
;; https://github.com/clojure-emacs/clj-refactor.el

(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :bind (:map clojure-mode-map
              ("C-:"   . iedit-mode)
              ("C-c :" . clojure-toggle-keyword-string))
  :hook ((clojure-mode . enable-paredit-mode)
         (clojure-mode . subword-mode)
         (clojure-mode . rainbow-delimiters-mode))
  :config
  (use-package clojure-mode-extra-font-locking))

(use-package cider
  :after clojure-mode
  :bind (("C-c M-B" . cider-browse-ns-all)
         ("C-c M-b" . browse-current-ns)
         ("C-c M-f" . cider-format-buffer)
         :map cider-repl-mode-map
         ("C-c C-o" . cider-repl-clear-buffer))
  :hook ((cider-repl-mode . paredit-mode)
         (cider-mode      . eldoc-mode))

  :config
  (defun browse-current-ns ()
    (interactive)
    (cider-browse-ns
     (with-current-buffer (current-buffer)
       (cider-current-ns))))

  (setq cider-repl-display-help-banner nil
        cider-repl-history-file "~/.emacs.d/cider-history"))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c TAB")
  (setq cljr-warn-on-eval nil))

;;;;;;;;;;;;;;;;;;;;;;;;;; Java ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/emacs-lsp/lsp-java

(use-package lsp-java
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

;;;;;;;;;;;;;;;;;;;;;;; Javascript ;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/jorgenschaefer/elpy

;; sudo pacman -Syu ipython
;; pip install --user rope flake8 importmagic black

(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :hook
  (elpy-mode . (lambda () (add-hook 'before-save-hook 'elpy-format-code)))
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"
        elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;;;;;;;;;;;;;;;;;;;;;;;;;; C/C++ ;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/MaskRay/emacs-ccls

;; sudo pacman -Syu clang
;; yay -Syu ccls
;; pip install --user compiledb cmake_format

(use-package ccls
  :hook ((c-mode c++-mode objc-mode)
         . (lambda () (require 'ccls) (lsp)))
  :bind (:map c-mode-base-map
              ("C-c C-c" . ff-find-other-file)
              ("C-z H"   . ccls-inheritance-hierarchy)
              ("C-z C"   . ccls-call-hierarchy)
              ("C-z M"   . ccls-member-hierarchy)
              ("C-z L"   . ccls-code-lens-mode))
  :init
  (setq flycheck-disabled-checkers
        '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :custom
  (ff-search-directories
   '("." "/usr/include" "/usr/local/include/*" ; original values
     "../include" "../../include/" "../src/*/*")))

(use-package clang-format
  :load-path "/usr/share/clang"
  :hook (before-save . clang-format-code)
  :config
  (defun clang-format-code ()
    (interactive)
    (when (derived-mode-p 'c-mode 'c++-mode 'objc-mode)
      (clang-format-buffer))))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :bind (:map cmake-mode-map
              ("C-c C-f" . cmake-format-buffer))
  :config
  (defun cmake-format-buffer ()
    (interactive)
    (let ((line (save-excursion (beginning-of-line) (1+ (count-lines 1 (point)))))
          (col  (save-excursion (goto-char (point)) (current-column)))
          (conf (current-window-configuration))
          (buf  (current-buffer)))

      (when-let (buf (get-buffer "*cmake-format*"))
        (kill-buffer buf))

      (with-current-buffer (get-buffer-create "*cmake-format*")
        (insert-buffer-substring buf)
        (let ((ret (shell-command-on-region
                    (point-min) (point-max)
                    "cmake-format - "
                    (current-buffer) nil
                    "*cmake-format*" t)))
          (cond
           ((zerop ret)
            (copy-to-buffer buf (point-min) (point-max))
            (kill-buffer)
            (set-window-configuration conf)
            (with-no-warnings (goto-line line))
            (move-to-column col t)
            (recenter-top-bottom))
           (t
            (special-mode)
            (select-window (get-buffer-window "*cmake-format*")))))))))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package eldoc-cmake
  :hook (cmake-mode . eldoc-cmake-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/dominikh/go-mode.el
;; https://github.com/dominikh/go-errcheck.el
;; https://github.com/golang/lint
;; https://github.com/rogpeppe/godef
;; https://github.com/nsf/gocode#emacs-setup
;; https://github.com/syohex/emacs-go-eldoc

;; go get -u golang.org/x/tools/cmd/...
;; go get -u golang.org/x/lint/golint
;; go get -u github.com/rogpeppe/godef/...
;; go get -u github.com/nsf/gocode
;; go get -u github.com/kisielk/errcheck

(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("M-."   . godef-jump)
              ("M-,"   . pop-tag-mark)
              ("C-c r" . go-rename))
  :hook (before-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports")
  (use-package go-guru
    :bind-keymap ("C-c C-c" . go-guru-map))
  (use-package go-rename)
  (use-package go-errcheck)
  (use-package golint)
  (use-package company-go
    :config (add-to-list 'company-backends 'company-go)))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;; Rust ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/rust-lang/rust-mode
;; https://github.com/flycheck/flycheck-rust
;; https://github.com/racer-rust/emacs-racer
;; https://github.com/kwrooijen/cargo.el

;; rustup component add rust-src rust-analysis
;; rustup component add rustfmt rls clippy
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
