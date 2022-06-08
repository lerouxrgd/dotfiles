;;; init.el --- Emacs initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Rebuild all packages:
;; M-: (byte-recompile-directory package-user-dir nil 'force)

;;; Code:

;;;;;;;;;;;;;;;;;;;; Package management ;;;;;;;;;;;;;;;;;;;;

;; As of Emacs 27 it is no longer necessary to call `package-initialize'
(setq package--initialized t)

(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :config (setq use-package-always-ensure t))

(use-package gnu-elpa-keyring-update)

(use-package quelpa-use-package
  :config
  (setq quelpa-update-melpa-p nil
        quelpa-checkout-melpa-p nil))

;;;;;;;;;;;;;;;;;;;;;;;; Interface ;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :init
  (load-theme 'doom-nova t)

  (custom-theme-set-faces
   'doom-nova
   `(hl-line ((t (:background ,(doom-color 'bg-alt))))))

  (use-package diff-mode
    :ensure nil
    :config
    (set-face-attribute 'diff-changed nil
                        :extend t
                        :background (doom-color 'bg-alt))))

;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 1.0))

(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes t
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-encoding nil)

  (defun doom-modeline-buffer-name ()
    (let ((doom-modeline-buffer-file-name-style 'truncate-with-project))
      (doom-modeline-buffer-file-name)))
  (setq frame-title-format '((:eval (doom-modeline-buffer-name)) " - %F")))

(use-package minions
  :config (minions-mode 1))

(use-package emacs
  :ensure nil
  :config
  (blink-cursor-mode    -1) ; Turn off blinking cursor
  (show-paren-mode       1) ; Highlight matching parenthesis
  (column-number-mode    1) ; Show column number
  (delete-selection-mode 1) ; Force selected text deletion

  (prefer-coding-system 'utf-8)
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'upcase-region   'disabled nil) ; Allow upcase selection
  (put 'downcase-region 'disabled nil) ; Allow downcase selection

  (setq
   inhibit-splash-screen      t
   inhibit-startup-message    t
   initial-major-mode         'fundamental-mode
   uniquify-buffer-name-style 'forward
   ring-bell-function         'ignore
   ad-redefinition-action     'accept
   split-height-threshold     nil)

  (setq-default
   fill-column      88  ; Right margin when filling paragraphs
   indent-tabs-mode nil ; Don't use hard tabs
   tab-width        4)

  ;; Setup line highlighting
  (global-hl-line-mode 1)
  (add-hook 'activate-mark-hook   (lambda () (global-hl-line-mode -1)))
  (add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))

  ;; Setup scrolling
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 10000
        scroll-preserve-screen-position 1
        mouse-wheel-scroll-amount '(1 ((shift) . 1)))

  ;; Setup local files
  (setq
   backup-directory-alist '(("." . "~/.emacs.d/backups"))
   custom-file            "~/.emacs.d/custom.el"
   auth-sources           '("~/.authinfo.gpg")
   create-lockfiles       nil   ; No need for ~ files when editing
   auto-save-default      nil)  ; No auto-save of file-visiting buffers
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (file-exists-p custom-file)
                (load custom-file)))))

;;;;;;;;;;;;;;;;;;;;; Custom functions ;;;;;;;;;;;;;;;;;;;;;

(use-package dash
  :config (global-dash-fontify-mode))

(defun unkillable-scratch-buffer ()
  "Disallow killing of scratch and delete its content instead."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (delete-region (point-min) (point-max))
             nil)
    t))

(defun switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

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

(defun scroll-up-preserve-line ()
  "Scrolling down."
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (scroll-up-command 1)))

(defun scroll-down-preserve-line ()
  "Scrolling up."
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (scroll-down-command 1)))

(defun mwheel-scroll-all-function-all (func &optional arg)
  "Scroll all windows based on FUNC with ARG."
  (if (and scroll-all-mode arg)
      (save-selected-window
        (walk-windows
         (lambda (win)
           (select-window win)
           (condition-case nil
               (funcall func arg)
             (error nil)))))
    (funcall func arg)))

(defun mwheel-scroll-all-scroll-up-all (&optional arg)
  "Scroll all windows up with ARG."
  (mwheel-scroll-all-function-all 'scroll-up arg))

(defun mwheel-scroll-all-scroll-down-all (&optional arg)
  "Scroll all windows down with ARG."
  (mwheel-scroll-all-function-all 'scroll-down arg))

(defun recenter-middle (&rest _)
  "Recenter to middle position, ignore params, useful when used as an advice."
  (recenter))

(defun defer-garbage-collection-h ()
  "Deactivate gc, used for minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  "Restore gc, used for minibuffer."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold (* 16 1024 1024)))))

(defun dedup-pop-to-mark (orig-fun &rest args)
  "Apply ORIG-FUN with ARGS while point is still at the same position."
  (let ((p (point)))
    (dotimes (_ 10)
      (when (= p (point))
        (apply orig-fun args)))))

(defun isearch-exit-mark-match ()
  "Exit isearch and mark the current match."
  (interactive)
  (isearch-exit)
  (push-mark isearch-other-end)
  (activate-mark))

(defun kill-word-no-ring (arg)
  "Delete characters backward until encountering the beginning of a word.
With ARG, do this that many times.  Does not push text to `kill-ring'."
  (interactive "p")
  (delete-region (point) (progn (forward-word (- arg)) (point))))

(use-package bind-map
  :preface (defvar my-keymap-key "C-x M-x")
  :config (bind-map my-keymap
                    :keys (my-keymap-key)
                    :bindings ("r" 'revert-all-file-buffers)))

(advice-add 'xref-pop-marker-stack      :after  'recenter-middle)
(advice-add 'xref-find-definitions      :after  'recenter-middle)
(advice-add 'occur-mode-goto-occurrence :after  'recenter-middle)
(advice-add 'compile-goto-error         :after  'recenter-middle)
(advice-add 'dedup-pop-to-mark          :after  'recenter-middle)
(advice-add 'pop-to-mark-command        :around 'dedup-pop-to-mark)

(add-hook 'kill-buffer-query-functions     'unkillable-scratch-buffer)
(add-hook 'before-save-hook                'delete-trailing-whitespace)
(add-hook 'occur-mode-find-occurrence-hook 'xref-pulse-momentarily)
(add-hook 'occur-mode-find-occurrence-hook 'recenter-middle)
(add-hook 'minibuffer-setup-hook           'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook            'restore-garbage-collection-h)

(define-key isearch-mode-map (kbd "<C-return>") 'isearch-exit-mark-match)

(global-set-key (kbd "S-C-<left>")    'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>")   'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")    'shrink-window)
(global-set-key (kbd "S-C-<up>")      'enlarge-window)
(global-set-key (kbd "C-x C-S-b")     'ibuffer)
(global-set-key (kbd "C-x C-z")       'repeat)
(global-set-key (kbd "<C-backspace>") 'kill-word-no-ring)
(global-set-key (kbd "<C-M-down>")    (kbd "C-u 2 C-v"))
(global-set-key (kbd "<C-M-up>")      (kbd "C-u 2 M-v"))
(global-set-key (kbd "M-DEL")         (kbd "C-u 0 C-k"))
(global-set-key (kbd "M-n")           'scroll-up-preserve-line)
(global-set-key (kbd "M-p")           'scroll-down-preserve-line)
(global-set-key (kbd "M-F")           'forward-whitespace)
(global-set-key (kbd "M-B")           'backward-whitespace)
(global-set-key (kbd "s-z")           'switch-to-scratch-buffer)
(global-set-key (kbd "C-;")           'toggle-comment-on-line)
(global-set-key (kbd "C-R")           'revert-all-file-buffers)
(global-set-key (kbd "C-z")            nil)

;;;;;;;;;;;;;;;;;;;;; General packages ;;;;;;;;;;;;;;;;;;;;;

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
         ([tab] . company-abort))
  :config
  (setq company-tooltip-align-annotations t
        company-selection-wrap-around t)
  (use-package company-quickhelp
    :config (company-quickhelp-mode)))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :bind (("C-x !" . flycheck-errors-buffer)
         :map flycheck-error-list-mode-map
         ("<C-return>" . flycheck-goto-error-kill-buffer))

  :init
  (defun flycheck-errors-buffer ()
    (interactive)
    (flycheck-list-errors)
    (select-window (get-buffer-window "*Flycheck errors*")))

  (defun flycheck-goto-error-kill-buffer ()
    (interactive)
    (flycheck-error-list-goto-error)
    (kill-buffer "*Flycheck errors*")
    (recenter-middle))

  :config
  (advice-add 'flycheck-error-list-goto-error :after 'recenter-middle)
  (setq flycheck-display-errors-function
        'flycheck-display-error-messages-unless-error-list)
  (add-to-list 'display-buffer-alist
               '("*Flycheck errors*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.20))))

(use-package ido-completing-read+
  :config
  (setq ido-auto-merge-work-directories-length -1
        ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1))

(use-package magit
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("<M-return>" . magit-diff-visit-file-other-window)
         :map magit-diff-mode-map
         ("<M-return>" . magit-diff-visit-file-other-window)
         ("<backtab>"  . magit-section-cycle-diffs))
  :config
  (transient-append-suffix 'magit-log "-A"
    '("-m" "Omit merge commits" "--no-merges"))
  (setq magit-diff-refine-hunk t)
  (advice-add 'magit-diff-visit-file-other-window :after 'recenter-middle)

  (use-package magit-todos
    :config (magit-todos-mode))

  (use-package magit-ediff
    :ensure nil
    :config (setq magit-ediff-dwim-show-on-hunks t))

  ;; git config --global github.user lerouxrgd
  ;; machine api.github.com login lerouxrgd^forge password token_xxx
  (use-package forge)
  (use-package code-review
    :config
    (setq code-review-auth-login-marker 'forge)))

(use-package git-timemachine
  :bind ("C-x G" . git-timemachine))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-force-faces t))

(use-package diffview
  :after magit
  :bind ((:map magit-mode-map
               ("C-d" . diffview-current))
         (:map diffview-mode-map
               ("l" . diffview-align-windows)))
  :config
  (defun diffview-align-windows ()
    (interactive)
    (let ((align-to-line (line-number-at-pos))
          (align-from-top (- (line-number-at-pos (point))
                             (line-number-at-pos (window-start)))))
      (when
          (cond
           ((string= (buffer-name (current-buffer))
                     diffview--minus-bufname)
            (switch-to-buffer-other-window diffview--plus-bufname))
           ((string= (buffer-name (current-buffer))
                     diffview--plus-bufname)
            (switch-to-buffer-other-window diffview--minus-bufname)))
        (goto-char (point-min))
        (forward-line (1- align-to-line))
        (recenter align-from-top)
        (other-window 1))))

  (setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all
        mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all))

(use-package exec-path-from-shell
  :if (memq system-type '(gnu gnu/linux darwin))
  :config (exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-arguments nil))

(use-package bash-completion
  :hook (shell-mode
         . (lambda () (local-set-key (kbd "TAB") 'company-indent-or-complete-common)))
  :config (bash-completion-setup))

(use-package recentf
  :ensure nil
  :defer 1
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 35
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package vlf
  :config (require 'vlf-setup))

(use-package editorconfig
  :config (editorconfig-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package selected
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("l" . downcase-region)
              ("w" . copy-to-register)
              ("m" . apply-macro-to-region-lines)
              ("d" . diffview-region))
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
   ("C-h" . mc-hide-unmatched-lines-mode)
   :map
   mc/keymap
   ("<backtab>" . mc/vertical-align-with-space))

  :hook ((multiple-cursors-mode . mc-selected-keys)
         (multiple-cursors-mode . global-hl-line-mode))

  :config
  (defun mc-selected-keys ()
    (if (bound-and-true-p multiple-cursors-mode)
        (progn
          (define-key selected-keymap (kbd "}") 'mc/cycle-forward)
          (define-key selected-keymap (kbd "{") 'mc/cycle-backward))
      (progn
        (define-key selected-keymap (kbd "}") nil)
        (define-key selected-keymap (kbd "{") nil)))))

(use-package visual-regexp
  :bind (("C-M-S" . vr/isearch-forward)
         ("C-M-R" . vr/isearch-backward)
         :map my-keymap
         ("M-%" . vr/query-replace)
         (">"   . vr/mc-mark))
  :config (use-package visual-regexp-steroids))

(use-package anzu
  :config (global-anzu-mode))

(use-package string-inflection
  :preface
  (which-key-add-key-based-replacements
    (concat my-keymap-key " i") "string-inflection")
  :bind ((:map my-keymap
               ("i c" . string-inflection-lower-camelcase) ; camelCase
               ("i p" . string-inflection-camelcase)       ; PascalCase
               ("i k" . string-inflection-kebab-case)      ; kebab-case
               ("i s" . string-inflection-underscore)      ; snake_case
               ("i u" . string-inflection-upcase))))       ; UPPER_CASE

(use-package smartparens
  :preface
  (which-key-add-key-based-replacements
    (concat my-keymap-key " p") "smartparens")
  :bind (("<C-s-right>" . sp-forward-slurp-sexp)
         ("<C-s-left>"  . sp-forward-barf-sexp)
         :map my-keymap
         ("p p" . sp-rewrap-sexp))
  :init
  (require 'smartparens-config)
  (smartparens-global-mode)
  :config
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
  (setq sp-highlight-pair-overlay nil))

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

(use-package undo-tree
  :bind ("C-M-/" . undo-tree-visualize)
  :config (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;; Navigation ;;;;;;;;;;;;;;;;;;;;;;;;

(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings))

(use-package buffer-move
  :bind (("<M-S-up>"    . buf-move-up)
         ("<M-S-down>"  . buf-move-down)
         ("<M-S-left>"  . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

(use-package nswbuff
  :bind (("C-<tab>"         . nswbuff-switch-to-next-buffer)
         ("<C-iso-lefttab>" . nswbuff-switch-to-previous-buffer)
         :map nswbuff-override-map
         ("k" . nswbuff-kill-this-buffer))
  :config
  (setq nswbuff-display-intermediate-buffers t
        nswbuff-status-window-layout 'minibuffer
        nswbuff-exclude-buffer-regexps
        '("^ " "^[*]" "null" "^Magit.*"))
  (custom-set-faces
   `(nswbuff-default-face
     ((t (:inherit default))))
   `(nswbuff-current-buffer-face
     ((t (:weight bold :background ,(doom-color 'blue) :foreground ,(doom-color 'bg)))))
   `(nswbuff-separator-face
     ((t (:foreground ,(doom-color 'orange)))))
   `(nswbuff-special-buffers-face
     ((t (:foreground ,(doom-color 'yellow)))))))

(use-package bufler
  :bind ("C-x C-b" . bufler))

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
     (-> (project-or-root) (split-string "/" "") (last) (car)))
    (treemacs-select-window))

  (use-package treemacs-magit)
  (doom-themes-treemacs-config)
  (setq treemacs-read-string-input 'from-minibuffer
        treemacs-collapse-dirs 7
        treemacs-file-follow-delay 0))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (use-package dired-subtree
    :bind (:map dired-mode-map
                ("<right>" . dired-subtree-insert)
                ("<left>"  . dired-subtree-remove)))
  (use-package dired-git-info
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))
  (use-package diredfl
    :config (diredfl-global-mode 1)))

(use-package symbol-overlay
  :bind (("C-z C-z" . symbol-overlay-mode)
         (:map my-keymap
               ("a"   . symbol-overlay-put)
               ("M-a" . symbol-overlay-remove-all)
               ("M-<" . symbol-overlay-switch-backward)
               ("M->" . symbol-overlay-switch-forward)
               )
         (:map symbol-overlay-mode-map
               ("C-M-n" . symbol-overlay-jump-next)
               ("C-M-p" . symbol-overlay-jump-prev)))
  :config
  (advice-add 'symbol-overlay-jump-first      :after 'recenter-middle)
  (advice-add 'symbol-overlay-jump-last       :after 'recenter-middle)
  (advice-add 'symbol-overlay-jump-next       :after 'recenter-middle)
  (advice-add 'symbol-overlay-jump-prev       :after 'recenter-middle)
  (advice-add 'symbol-overlay-switch-backward :after 'recenter-middle)
  (advice-add 'symbol-overlay-switch-forward  :after 'recenter-middle)
  (setq symbol-overlay-idle-time 0.2))

(use-package imenu-list
  :config
  (global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
  (setq imenu-list-focus-after-activation t))

(use-package avy
  :bind (("C-\\"  . avy-goto-word-1)
         ("M-g g". avy-goto-line)))

(use-package goto-line-preview
  :config
  (defun with-linum (f &rest args)
    (linum-mode 1)
    (unwind-protect
        (apply f args)
      (linum-mode -1)))

  (advice-add 'goto-line-preview :around 'with-linum)
  (advice-add 'goto-line-preview--do :after 'recenter-middle)
  (setq goto-line-preview-after-hook 'recenter-middle)

  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package origami
  :bind-keymap ("M-o" . origami-mode-map)
  :bind (:map origami-mode-map
              ("M-o M-o" . origami-recursively-toggle-node)
              ("M-o M-." . origami-forward-fold)
              ("M-o M-," . origami-previous-fold)
              ("M-o M-r" . origami-reset)
              ("M-o O"   . origami-show-only-node)
              ("M-o <"   . origami-close-all-nodes)
              ("M-o >"   . origami-open-all-nodes)
              ("M-o /"   . origami-undo)
              ("M-o ?"   . origami-redo))
  :config (global-origami-mode))

(use-package fold-this
  :after selected
  :bind ((:map selected-keymap
               ("o" . fold-this-or-rest))
         (:map my-keymap
               ("M-o" . fold-this-unfold-all)))
  :init
  (defun fold-this-or-rest (&optional arg)
    (interactive "P")
    (if (not arg)
        (fold-this (region-beginning) (region-end))
      ;; fold-all-but-this
      (fold-this (point-min) (region-beginning))
      (fold-this (region-end) (point-max))
      (deactivate-mark)))
  :config
  (custom-set-faces
   `(fold-this-overlay ((t (:foreground ,(doom-color 'white)))))))

;; (use-package hideshow
;;   :hook (prog-mode . hs-minor-mode)
;;   :bind (("M-o" . hs-toggle-hiding)
;;          :map my-keymap
;;          ("M-o" . hs-show-all)))

(use-package helm
  :preface (require 'helm-config)
  :bind (("C-x x" . helm-command-prefix)
         :map helm-command-map
         ("SPC" . helm-all-mark-rings)
         ("y"   . helm-register)
         ("m"   . helm-semantic-or-imenu)
         ("c"   . helm-comint-input-ring)
         ("."   . helm-etags-select))
  :config
  (require 'helm-ring)
  (helm-autoresize-mode t)
  (setq helm-always-two-windows t
        helm-split-window-inside-p t))

;; sudo pacman -Syu fd
(use-package helm-fd
  :after helm
  :ensure nil
  :bind (:map helm-command-map
              ("/" . helm-fd-project))
  :config
  (defun helm-fd-project ()
    (interactive)
    (let ((directory (or (cdr (project-current))
                         (with-current-buffer "*scratch*" default-directory))))
      (helm-fd-1 directory))))

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
(use-package helm-ag
  :defer 1
  :bind (("C-c c" . (lambda () (interactive) (helm-do-ag (project-or-root))))
         ("C-c C" . (lambda () (interactive) (helm-do-ag default-directory))))
  :config
  (setq helm-ag-base-command "rg --no-heading"
        helm-ag-success-exit-status '(0 2)
        helm-ag-insert-at-point 'symbol))

(use-package helm-xref
  :config (advice-add 'helm-xref-goto-xref-item :after 'recenter-middle))

(use-package dumb-jump
  :config
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions)
        dumb-jump-selector 'helm)
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t))

;;;;;;;;;;;;;;;;;;;;;;;;;;; LSP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-z"
        lsp-enable-symbol-highlighting nil
        lsp-signature-doc-lines 2))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind ((:map lsp-command-map
               ("!" . lsp-ui-flycheck-list)
               ("m" . lsp-ui-imenu)
               ("z" . lsp-ui-doc-mode)
               ("." . lsp-find-definition)
               ("?" . lsp-ui-peek-find-references))
         (:map lsp-ui-flycheck-list-mode-map
               ("C-<return>" . lsp-ui-flycheck-goto-error-kill-buffer)))
  :config
  (defun lsp-ui-flycheck-goto-error-kill-buffer ()
    (interactive)
    (lsp-ui-flycheck-list--visit)
    (kill-buffer "*lsp-diagnostics*")
    (recenter-middle))
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'at-point))

(use-package lsp-treemacs :after (lsp-mode treemacs))

(use-package lsp-origami
  :after lsp-mode
  :bind (:map lsp-command-map
              ("To" . lsp-origami-mode)))

(use-package helm-lsp
  :after lsp-mode
  :bind
  (:map lsp-command-map
        ("aa" . helm-lsp-code-actions)
        ("ga" . (lambda () (interactive) (helm-lsp-workspace-symbol t)))
        ("gA" . (lambda () (interactive) (helm-lsp-global-workspace-symbol t)))))

;;;;;;;;;;;;;;;;;;;; Simple formatting ;;;;;;;;;;;;;;;;;;;;

(use-package highlight-indent-guides
  :config (setq highlight-indent-guides-method 'character))

(use-package yaml-mode
  :mode "\\.yaml\\'"
  :hook (yaml-mode
         . (lambda () (local-set-key (kbd "<backtab>") 'company-complete))))

;; npm install -g yaml-language-server
(use-package yaml-mode-ext
  :quelpa (yaml-mode-ext :fetcher github :repo "lerouxrgd/yaml-mode-ext")
  :hook (yaml-mode . (lambda () (require 'yaml-mode-ext)))
  :bind (:map yaml-mode-map
              ("C-c C-c RET" . yaml-reset-schemas)
              ("C-c C-c k"   . yaml-set-schema-k8s))
  :config
  (defvar lsp-yaml-schemas)

  (defun yaml-reset-schemas ()
    (interactive)
    (setq lsp-yaml-schemas (make-hash-table))
    (message "LSP yaml schemas reset"))

  (defun yaml-set-schema-k8s ()
    (interactive)
    (setq lsp-yaml-schemas '(:kubernetes "*.yml"))
    (message "LSP yaml schema set to: kubernetes")))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.avsc\\'" . json-mode))
  :config (setq js-indent-level 2))

(use-package protobuf-mode)

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

(use-package rfc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;; Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  (use-package company-terraform
    :config (company-terraform-init)))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package restclient
  :config
  (use-package restclient-helm)
  (use-package company-restclient
    :hook (restclient-mode
           . (lambda () (add-to-list 'company-backends 'company-restclient)))
    :bind (:map restclient-mode-map
                ("C-c TAB" . company-complete))))

;;;;;;;;;;;;;;;;;;;;;;;; Scientific ;;;;;;;;;;;;;;;;;;;;;;;;

;; sudo pacman -Syu texlive-core
(use-package latex-preview-pane
  :config (setq pdf-latex-command "xelatex"))

;; sudo pacman -Syu tk gcc-fortran
;; install.packages("lintr")
(use-package ess
  :mode (("\\.[rR]\\'" . ess-r-mode)
         ("\\.jl\\'"   . julia-mode))
  :hook (ess-mode
         . (lambda () (local-set-key (kbd "TAB") 'company-indent-or-complete-common)))
  :config
  (setq ess-use-flymake nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :hook (emacs-lisp-mode . (lambda () (setq flycheck-check-syntax-automatically
                                            '(mode-enabled save)))))

(use-package flycheck-package
  :no-require t)

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode       . enable-paredit-mode)))

(use-package rainbow-delimiters)

;; pamac install chez-scheme
(use-package geiser
  :preface (setq geiser-active-implementations '(chez))
  :hook ((scheme-mode      . enable-paredit-mode)
         (geiser-repl-mode . enable-paredit-mode)))

;; pamac install janet-lang
(use-package janet-mode)

;;;;;;;;;;;;;;;;;;;;;;;;; Clojure ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/clojure-emacs/clojure-mode
;; https://github.com/clojure-emacs/cider
;; https://github.com/clojure-emacs/clj-refactor.el
;; https://github.com/borkdude/flycheck-clj-kondo

;; https://github.com/technomancy/leiningen
;; https://github.com/thheller/shadow-cljs
;; https://github.com/borkdude/clj-kondo
;; https://github.com/snoe/clojure-lsp
;; https://github.com/weavejester/cljfmt

;; sudo pacman -Syu leiningen
;; pamac install nodejs-shadow-cljs
;; pamac install clj-kondo-bin
;; pamac install clojure-lsp-bin

(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :hook ((clojure-mode . enable-paredit-mode)
         (clojure-mode . subword-mode)
         (clojure-mode . rainbow-delimiters-mode))
  :config
  (use-package clojure-mode-extra-font-locking)
  (use-package flycheck-clj-kondo))

(use-package cider
  :after clojure-mode
  :bind (("C-c M-f" . cider-format-buffer)
         :map cider-repl-mode-map
         ("C-c C-o" . cider-repl-clear-buffer))
  :hook ((cider-repl-mode . paredit-mode)
         (cider-mode      . eldoc-mode))
  :config
  (advice-add 'cider-find-var :after 'recenter-middle)
  (setq cider-repl-display-help-banner nil
        cider-prompt-for-symbol nil
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-tab-command
        (lambda () (company-indent-or-complete-common (symbol-at-point)))))

(use-package helm-cider
  :after clojure-mode
  :hook (clojure-mode . helm-cider-mode)
  :bind (("C-c s" . helm-cider-cheatsheet)
         ("C-c S" . helm-cider-spec))
  :config
  (setq helm-cider--doc-actions
        (helm-make-actions
         "Clojuredocs"     (wrap-helm-cider-action cider-clojuredocs-lookup)
         "CiderDoc"        (wrap-helm-cider-action cider-doc-lookup)
         "Find definition" (wrap-helm-cider-action helm-cider--find-var))))

(use-package clj-refactor
  :pin melpa-stable
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-SPC")
  (setq cljr-warn-on-eval nil))

;;;;;;;;;;;;;;;;;;;;;;;; Java/Scala ;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/emacs-lsp/lsp-java
;; https://github.com/scalameta/metals

;; pamac install jdtls
;; pamac install metals

(use-package lsp-java
  :defer 1
  :config (add-hook 'java-mode-hook 'lsp))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;;;;;;;;;;;;;;;;;;;;;;;; Javascript ;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/codesuki/add-node-modules-path

(use-package js
  :hook (js-mode . (lambda () (local-set-key (kbd "M-.") 'xref-find-definitions))))

(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

;;;;;;;;;;;;;;;;;;;;;;;;;; Erlang ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://erlang.org/doc/man/erlang.el.html
;; https://github.com/erlang/rebar3
;; https://github.com/erlang-ls/erlang_ls

;; sudo pacman -Syu erlang
;; pamac install rebar3
;; pamac install erlang_ls-git

(use-package erlang
  :no-require t
  :config (require 'erlang-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lua ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lua-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;; Haxe ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/emacsorphanage/haxe-mode
;; https://github.com/vshaxe/haxe-language-server
;; https://deepnight.net/tutorial/a-quick-guide-to-installing-haxe/

;; sudo pacman -Syu haxe
;; pamac install hashlink
;; mkdir -p ~/.haxe-language-server/bin/

(use-package haxe-mode
  :config (add-hook 'haxe-mode-hook 'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/jorgenschaefer/elpy
;; https://github.com/galaunay/poetry.el
;; https://github.com/brotzeit/pippel
;; https://github.com/millejoh/emacs-ipython-notebook

;; sudo pacman -Syu ipython pyenv jupyter
;; pip install --user jedi black flake8 pylint

(use-package elpy
  :pin melpa-stable
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :hook
  ((python-mode          . highlight-indent-guides-mode)
   (elpy-mode            . (lambda () (add-hook 'before-save-hook 'elpy-format-code)))
   (inferior-python-mode . (lambda () (local-set-key (kbd "TAB") 'company-complete))))
  :bind (:map elpy-mode-map
              ("<C-down>" . forward-paragraph)
              ("<C-up>"   . backward-paragraph))
  :config
  (advice-add 'elpy-format-code :after 'recenter-middle)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"
        ;; elpy-syntax-check-command "pylint"
        elpy-rpc-virtualenv-path 'current
        elpy-modules (->> elpy-modules
                          (delq 'elpy-module-flymake)
                          (delq 'elpy-module-highlight-indentation))))

(use-package poetry
  :hook (python-mode . (lambda () (local-set-key (kbd "C-c p") 'poetry))))

(use-package pippel
  :after python-mode)

(use-package ein)

;;;;;;;;;;;;;;;;;;;;;;;;;; C/C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/MaskRay/emacs-ccls

;; sudo pacman -Syu clang
;; pamac install ccls
;; pip install --user compiledb cmake_format

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) . setup-ccls)
  :bind (:map c-mode-base-map
              ("C-c C-c" . ff-find-other-file))
  :init
  (setq flycheck-disabled-checkers
        '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :config
  (defun setup-ccls ()
    (when (derived-mode-p 'c-mode 'c++-mode 'objc-mode)
      (clang-format-buffer)
      (require 'ccls)
      (lsp)
      (define-key lsp-command-map (kbd "ci") 'ccls-inheritance-hierarchy)
      (define-key lsp-command-map (kbd "cc") 'ccls-call-hierarchy)
      (define-key lsp-command-map (kbd "cm") 'ccls-member-hierarchy)
      (define-key lsp-command-map (kbd "cl") 'ccls-code-lens-mode)
      (which-key-add-major-mode-key-based-replacements
        major-mode (concat lsp-keymap-prefix " c") "ccls")))
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

(use-package glsl-mode
  :bind (:map glsl-mode-map
              ("C-c C-c" . ff-find-other-file)
              ("C-?"     . glsl-find-man-page)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    :config (define-key go-mode-map (kbd "C-c C-c") 'go-guru-map))
  (use-package go-rename)
  (use-package go-errcheck)
  (use-package golint)
  (use-package company-go
    :hook (go-mode . (lambda () (add-to-list 'company-backends 'company-go)))))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Rust ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/rust-lang/rust-mode
;; https://github.com/flycheck/flycheck-rust
;; https://github.com/kwrooijen/cargo.el

;; rustup toolchain install stable
;; rustup component add rust-src rust-analysis

;; sudo pacman -Syu rust-analyzer sccache lld
;; sudo pacman -Syu cargo-edit cargo-outdated
(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-o" . rust-occur-definitions))
  :hook (rust-mode . lsp)
  :config
  (setq rust-format-on-save t)
  (defun rust-occur-definitions ()
    (interactive)
    (let ((list-matching-lines-face nil))
      (occur (concat "^\s*\\("
                     "\\(pub.*?\s\\|\\)mod\\|"
                     "\\(pub.*?\s\\|\\)type\\|"
                     "\\(pub.*?\s\\|\\)struct\\|"
                     "\\(pub.*?\s\\|\\)enum\\|"
                     "\\(pub.*?\s\\|unsafe\s\\|extern.+?\s\\|const\s\\|async\s\\|\\)+fn\\|"
                     "\\(pub.*?\s\\|unsafe\s\\|\\)trait\\|"
                     "\\(pub.*?\s\\|\\)const\\|"
                     "\\(pub.*?\s\\|\\)static\sref\\|"
                     "impl\\(<.+?>\\|\\)\\|"
                     "lazy_static\!"
                     "\\)\s")))
    (let ((window (get-buffer-window "*Occur*")))
      (if window
          (select-window window)
        (switch-to-buffer "*Occur*"))
      (setq-local list-matching-lines-face nil)
      (put 'list-matching-lines-face 'permanent-local t))))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq cargo-process--command-clippy "clippy"))

(use-package ron-mode)

;; cargo install pesta
(use-package pest-mode
  :hook (pest-mode . flymake-mode))

(use-package wgsl-mode
  :quelpa (wgsl-mode :fetcher github :repo "acowley/wgsl-mode"))

;;; init.el ends here
