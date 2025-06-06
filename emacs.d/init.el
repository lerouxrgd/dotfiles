;;; init.el --- Emacs initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Rebuild all packages:
;; M-: (byte-recompile-directory package-user-dir nil 'force)

;;; Code:

;;;;;;;;;;;;;;;;;;;; Package management ;;;;;;;;;;;;;;;;;;;;

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(use-package use-package
  :config (setq use-package-always-ensure t))

(use-package gnu-elpa-keyring-update)

(use-package quelpa
  :config
  (setq quelpa-update-melpa-p nil
        quelpa-checkout-melpa-p nil
        quelpa-melpa-recipe-stores '())
  (use-package quelpa-use-package
    :config (quelpa-use-package-activate-advice)))

;;;;;;;;;;;;;;;;;;;;;;;; Interface ;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :init
  (setq doom-theme 'doom-nova)
  (load-theme doom-theme t)
  (custom-theme-set-faces
   doom-theme
   `(hl-line ((t (:background ,(doom-color 'bg-alt)))))))

;; M-x nerd-icons-install-fonts
(use-package nerd-icons)

(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes t
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-encoding nil)

  (custom-set-faces
   `(mode-line-inactive
     ((t (:background ,(doom-blend 'grey 'bg 0.2))))))

  (defun doom-modeline-buffer-name ()
    (let ((doom-modeline-buffer-file-name-style 'truncate-with-project))
      (doom-modeline-buffer-file-name)))
  (setq frame-title-format '((:eval (doom-modeline-buffer-name)) " - %F")))

(use-package minions
  :config (minions-mode 1))

(use-package popup
  :config
  (custom-set-faces
   `(popup-tip-face
     ((t (:background "#35424a" :foreground ,(doom-color 'white)))))))

(use-package emacs
  :ensure nil
  :config
  (blink-cursor-mode    -1)             ; Turn off blinking cursor
  (show-paren-mode       1)             ; Highlight matching parenthesis
  (column-number-mode    1)             ; Show column number
  (delete-selection-mode 1)             ; Force selected text deletion

  (prefer-coding-system 'utf-8)
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'upcase-region   'disabled nil)  ; Allow upcase selection
  (put 'downcase-region 'disabled nil)  ; Allow downcase selection

  (setq
   inhibit-splash-screen                    t
   inhibit-startup-message                  t
   native-comp-async-report-warnings-errors 'silent
   initial-major-mode                       'fundamental-mode
   uniquify-buffer-name-style               'forward
   ring-bell-function                       'ignore
   ad-redefinition-action                   'accept
   eldoc-documentation-strategy             'eldoc-documentation-compose-eagerly
   mode-require-final-newline               nil
   split-height-threshold                   nil)

  (setq-default
   imenu-sort-function 'imenu--sort-by-position
   fill-column      88     ; Right margin when filling paragraphs
   indent-tabs-mode nil    ; Don't use hard tabs
   tab-width        4)

  ;; Setup line highlighting
  (global-hl-line-mode 1)
  (add-hook 'activate-mark-hook   (lambda () (global-hl-line-mode -1)))
  (add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))

  ;; Setup scrolling
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 10000
        scroll-preserve-screen-position 'always
        mouse-wheel-scroll-amount '(1 ((shift) . 1)))

  ;; Setup local files
  (setq
   backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
   custom-file            (concat user-emacs-directory "custom.el")
   auth-sources           '("~/.authinfo.gpg")
   create-lockfiles       nil           ; No need for ~ files when editing
   auto-save-default      nil)          ; No auto-save of file-visiting buffers
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
  (or (vc-root-dir)
      (caddr (project-current))
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
  (select-window (selected-window)) (recenter))

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

(defun isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))

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
(advice-add 'compile-goto-error         :after  'recenter-middle)
(advice-add 'dedup-pop-to-mark          :after  'recenter-middle)
(advice-add 'pop-to-mark-command        :around 'dedup-pop-to-mark)

(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)
(add-hook 'before-save-hook            'delete-trailing-whitespace)
(add-hook 'minibuffer-setup-hook       'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook        'restore-garbage-collection-h)
(add-hook 'isearch-mode-hook           'isearch-with-region)

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
(global-set-key (kbd "C-S-R")         'revert-all-file-buffers)
(global-set-key (kbd "C-z")            nil)

;;;;;;;;;;;;;;;;;;;;; General packages ;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package smex
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory "smex-items"))
  (smex-initialize))

(use-package company
  :hook (after-init . global-company-mode)
  :bind (("TAB" . company-indent-or-complete-common)
         :map company-active-map
         ([tab] . company-abort))
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-minimum-width 50
        company-tooltip-maximum-width 100
        company-selection-wrap-around t)
  (use-package company-posframe
    :init (setq company-posframe-quickhelp-show-header nil)
    :config
    (company-posframe-mode 1)
    (define-key company-posframe-active-map (kbd "<C-return>")
               'company-posframe-quickhelp-toggle)
    (define-key company-posframe-active-map (kbd "C-<up>")
               'company-posframe-quickhelp-scroll-down)
    (define-key company-posframe-active-map (kbd "C-<down>")
               'company-posframe-quickhelp-scroll-up)))

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
         ("C-x G" . magit-status-here)
         :map magit-status-mode-map
         ("<M-return>" . magit-diff-visit-file-other-window)
         :map magit-diff-mode-map
         ("<M-return>" . magit-diff-visit-file-other-window)
         ("<backtab>"  . magit-section-cycle-diffs)
         :map my-keymap
         ("d" . magit-diff-buffer-file)
         ("g" . magit-status-current-window)
         ("G" . magit-status-here-current-window))
  :config
  (transient-append-suffix 'magit-log "-A"
    '("-m" "Omit merge commits" "--no-merges"))

  (setq magit-diff-refine-hunk t)
  (advice-add 'magit-diff-visit-file-other-window :after 'recenter-middle)
  (advice-add 'magit-diff-buffer-file :after 'recenter-middle)
  (advice-add 'magit-status-here :after 'recenter-middle)

  (defun magit-status-current-window ()
    (interactive)
    (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
      (magit-status)))

  (defun magit-status-here-current-window ()
    (interactive)
    (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
      (magit-status-here)))

  (use-package magit-todos
    :config
    (magit-todos-mode)
    (setq magit-todos-nice nil))

  (use-package magit-ediff
    :ensure nil
    :config
    (setq magit-ediff-dwim-show-on-hunks t)))

(use-package forge
  :after magit)

;; git config --global github.user lerouxrgd
;; (~/.authinfo.gpg) machine api.github.com login lerouxrgd^forge password token_xxx
(use-package code-review
  ;; FIXME: https://github.com/wandersoncferreira/code-review/issues/245
  ;; FIXME: https://github.com/wandersoncferreira/code-review/pull/246
  :quelpa (code-review
           :fetcher github
           :repo "phelrine/code-review"
           :branch "fix/closql-update")
  :ensure nil
  :after magit
  :bind ((:map magit-status-mode-map
               ("C-c r" . code-review-forge-pr-at-point))
         (:map code-review-mode-map
               ("<M-return>" . magit-diff-visit-file-other-window)))
  :config
  (setq code-review-auth-login-marker 'forge))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :bind (("C-x C-<up>"   . git-gutter:previous-hunk)
         ("C-x C-<down>" . git-gutter:next-hunk)
         ("C-x D"        . git-gutter:popup-hunk))
  :config
  (advice-add 'git-gutter:previous-hunk :after 'recenter-middle)
  (advice-add 'git-gutter:next-hunk :after 'recenter-middle)
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package git-timemachine
  :bind ("C-x H" . git-timemachine))

(use-package git-messenger
  :quelpa (git-messenger :fetcher github :repo "cnsunyour/git-messenger")
  :bind ("C-x M" . git-messenger:popup-message)
  :config
  (defun git-messenger:popup-show ()
    (interactive)
    (magit-show-commit git-messenger:last-commit-id)
    (recenter-middle)
    (git-messenger:popup-close))
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

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

(use-package selected
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("l" . downcase-region)
              ("w" . copy-to-register)
              ("m" . apply-macro-to-region-lines)
              ("d" . diffview-region))
  :init (selected-global-mode))

(use-package expreg
  :bind (:map selected-keymap
              ("=" . expreg-expand)
              ("-" . expreg-contract)))

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

(use-package phi-search
  :init
  (add-hook 'multiple-cursors-mode-enabled-hook
            (lambda ()
              (interactive)
              (global-set-key (kbd "C-s") 'phi-search)
              (global-set-key (kbd "C-r") 'phi-search-backward)))
  (add-hook 'multiple-cursors-mode-disabled-hook
            (lambda ()
              (interactive)
              (global-set-key (kbd "C-s") 'isearch-forward)
              (global-set-key (kbd "C-r") 'isearch-backward))))

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

(use-package vundo
  :bind ("C-M-/" . vundo))

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

(use-package popper
  :bind ("C-`" . popper-toggle)
  :init
  (setq popper-reference-buffers '("\\*git-gutter:diff\\*"))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . (lambda ()
                         (rename-buffer
                          (generate-new-buffer-name
                           (format "*dired: %s*" (buffer-name)))))))
  :config
  (use-package dired-git-info
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))
  (use-package diredfl
    :config (diredfl-global-mode 1)))

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :bind (("C-x t" . dirvish-side)
         :map dirvish-mode-map
         ("TAB"             . dirvish-subtree-toggle)
         ("."               . dired-create-empty-file)
         ("C-<tab>"         . (lambda () (interactive)))
         ("<C-iso-lefttab>" . (lambda () (interactive)))
         ("<return>"        . (lambda () (interactive)
                                (dired-find-file)
                                (let ((window (get-buffer-window)))
                                  (dirvish-side)
                                  (when window (select-window window)))))
         ("C-<return>"      . (lambda () (interactive)
                                (dired-find-file)
                                (dirvish-side)
                                (dirvish-quit))))
  :config
  (defun dirvish-side-selectable-window (orig-fun &rest args)
    (-if-let (win (dirvish-side--session-visible-p))
        (progn
          (set-window-parameter win 'no-other-window nil)
          (let ((res (apply orig-fun args)))
            (set-window-parameter win 'no-other-window t)
            res))
      (apply orig-fun args)))
  (advice-add 'windmove-do-window-select :around 'dirvish-side-selectable-window)
  (setq delete-by-moving-to-trash t
        dirvish-reuse-session nil
        dirvish-mode-line-height 23
        dirvish-header-line-height 20
        dirvish-attributes '(nerd-icons
                             file-time
                             subtree-state
                             vc-state)
        dirvish-subtree-state-style 'nerd
        dired-listing-switches (concat "-l "
                                       "--almost-all "
                                       "--human-readable "
                                       "--group-directories-first "
                                       "--no-group ")
        dirvish-side-window-parameters '((no-delete-other-windows . t)
                                         (no-other-window . t)))
  (dirvish-side-follow-mode))

(use-package occur
  :ensure nil
  :bind (:map occur-mode-map
              ("C-<return>" . occur-mode-goto-occurrence-kill-buffer))
  :hook ((occur-mode-find-occurrence . xref-pulse-momentarily)
         (occur-mode-find-occurrence . recenter-middle))
  :init
  (defun occur-mode-goto-occurrence-kill-buffer ()
    (interactive)
    (occur-mode-goto-occurrence-other-window)
    (other-window 1)
    (quit-window)))

(use-package symbol-overlay
  :bind (("C-z C-z" . symbol-overlay-mode)
         (:map my-keymap
               ("a"   . symbol-overlay-put)
               ("M-a" . symbol-overlay-remove-all)
               ("M-<" . symbol-overlay-switch-backward)
               ("M->" . symbol-overlay-switch-forward))
         (:map symbol-overlay-mode-map
               ("C-M-n" . symbol-overlay-jump-next)
               ("C-M-p" . symbol-overlay-jump-prev)))
  :init
  (custom-theme-set-faces
   doom-theme
   `(symbol-overlay-default-face ((t (:inherit symbol-overlay-face-1)))))
  :config
  (advice-add 'symbol-overlay-jump-first      :after 'recenter-middle)
  (advice-add 'symbol-overlay-jump-last       :after 'recenter-middle)
  (advice-add 'symbol-overlay-jump-next       :after 'recenter-middle)
  (advice-add 'symbol-overlay-jump-prev       :after 'recenter-middle)
  (advice-add 'symbol-overlay-switch-backward :after 'recenter-middle)
  (advice-add 'symbol-overlay-switch-forward  :after 'recenter-middle)
  (setq symbol-overlay-idle-time 0.2))

(use-package avy
  :bind (("C-\\"  . avy-goto-word-1)
         ("M-g g". avy-goto-line)))

(use-package goto-line-preview
  :config
  (defun with-line-numbers (f &rest args)
    (display-line-numbers-mode 1)
    (unwind-protect
        (apply f args)
      (display-line-numbers-mode -1)))
  (advice-add 'goto-line-preview :around 'with-line-numbers)
  (advice-add 'goto-line-preview--do :after 'recenter-middle)
  (setq goto-line-preview-after-hook 'recenter-middle
        goto-line-preview-hl-duration 0)
  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package helm
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
  (setq helm-fd-mode-line-function nil)
  (defun helm-fd-project ()
    (interactive)
    (let ((directory (project-or-root)))
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
  :bind (("C-c c" . (lambda () (interactive) (helm-do-ag (project-or-root))))
         ("C-c C" . (lambda () (interactive) (helm-do-ag default-directory))))
  :config
  (advice-add 'helm-ag--edit :after 'selected-minor-mode)
  (setq helm-ag-base-command "rg -S --no-heading"
        helm-ag-success-exit-status '(0 2)
        helm-ag-insert-at-point 'symbol
        helm-ag-show-status-function nil))

(use-package helm-xref
  :config (advice-add 'helm-xref-goto-xref-item :after 'recenter-middle))

(use-package dumb-jump
  :config
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions)
        dumb-jump-selector 'helm)
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t))

(use-package ov
  :bind (("M-o M->" . ov-goto-next)
         ("M-o M-<" . ov-goto-prev)
         ("M-o DEL" . ov-clear))
  :config
  (advice-add 'ov-goto-next :after 'recenter-middle)
  (advice-add 'ov-goto-prev :after 'recenter-middle))

;;;;;;;;;;;;;;;;;;;;;;; Tree Sitter ;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist '())
  (defun ensure-treesit (grammar-spec)
    (add-to-list 'treesit-language-source-alist grammar-spec)
    (unless (treesit-language-available-p (car grammar-spec))
      (treesit-install-language-grammar (car grammar-spec)))))

(use-package treesit-fold
  ;; FIXME: https://github.com/emacs-tree-sitter/treesit-fold/issues/11
  :quelpa (treesit-fold :fetcher github :repo "emacs-tree-sitter/treesit-fold")
  :bind (("M-o M-o" . treesit-fold-toggle)
         ("M-o o"   . treesit-fold-open-all)
         ("M-o O"   . treesit-fold-close-all))
  :config (global-treesit-fold-mode))

(use-package combobulate
  :quelpa (combobulate :fetcher github :repo "mickeynp/combobulate"))

;;;;;;;;;;;;;;;;;;;;;;;;;;; LSP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-z"
        lsp-enable-symbol-highlighting nil
        lsp-lens-enable nil
        lsp-signature-doc-lines 2
        lsp-imenu-sort-methods '(position)
        company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

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
        lsp-ui-imenu-auto-refresh t
        lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'at-point))

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

;; sudo pacman -Syu taplo-cli
(use-package toml-ts-mode
  :init (ensure-treesit '(toml "https://github.com/tree-sitter/tree-sitter-toml"))
  :hook
  ((toml-ts-mode . lsp-deferred)
   (toml-ts-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer t))))
  :mode "\\.toml\\'")

(use-package json-ts-mode
  :init (ensure-treesit '(json "https://github.com/tree-sitter/tree-sitter-json"))
  :mode (("\\.json\\'" . json-ts-mode)
         ("\\.avsc\\'" . json-ts-mode))
  :config
  (use-package json-mode)
  (setq json-ts-mode-map json-mode-map))

(use-package protobuf-mode)

(use-package mustache-mode)

;; sudo pacman -Syu marked
;; pipx install grip
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

(use-package sql-cassandra)

;;;;;;;;;;;;;;;;;;;;;;;;;;; Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sudo pacman -Syu yaml-language-server
(use-package yaml-ts-mode
  :init (ensure-treesit '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'"
  :hook (yaml-ts-mode . combobulate-mode)
  :bind (:map yaml-ts-mode-map
              ("C-c C-s" . helm-lsp-yaml-select-buffer-schema))
  :config
  (setq lsp-yaml-format-enable nil)
  (defun helm-lsp-yaml-select-buffer-schema ()
    "Run lsp-yaml-select-buffer-schema in temporary helm-mode."
    (interactive)
    (unwind-protect
        (progn
          (ido-everywhere -1)
          (helm-mode 1)
          (lsp-yaml-select-buffer-schema))
      (helm-mode -1)
      (ido-everywhere 1))))

;; pamac install dockerfile-language-server
(use-package dockerfile-ts-mode
  :init (ensure-treesit '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"))
  :mode "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'")

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  (use-package company-terraform
    :config (company-terraform-init)))

;;;;;;;;;;;;;;;;;;;;;;;; Scientific ;;;;;;;;;;;;;;;;;;;;;;;;

;; sudo pacman -Syu texlive-core texlive-latexextra
;; sudo pacman -Syu texlive-fontsrecommended texlive-fontsextra
(use-package latex-preview-pane
  :config (setq pdf-latex-command "xelatex"))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
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

;; sudo pacman -Syu leiningen
;; pamac install nodejs-shadow-cljs clj-kondo-bin clojure-lsp-bin
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
        cider-repl-history-file (concat user-emacs-directory "cider-history")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lua ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sudo pacman -Syu lua-language-server
(use-package lua-ts-mode
  ;; FIXME: lua-ts-mode will be available in Emacs 30
  :quelpa (lua-ts-mode :fetcher file :path "~/.emacs.d/lua-ts-mode.el")
  :init (ensure-treesit '(lua "https://github.com/MunifTanjim/tree-sitter-lua"))
  :mode "\\.lua\\'"
  :hook
  ((lua-ts-mode . lsp-deferred)
   (lua-ts-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sudo pacman -Syu python-pipx pyenv ipython
(use-package python
  :init (ensure-treesit '(python "https://github.com/tree-sitter/tree-sitter-python"))
  :mode ("\\.py[iw]?\\'" . python-ts-mode)
  :bind (:map python-ts-mode-map
              ("C-c C-o" . python-occur-definitions))
  :hook (python-ts-mode . combobulate-mode)
  :config
  (defun python-occur-definitions ()
    "Display an occur buffer of all definitions in the current buffer."
    (interactive)
    (let ((list-matching-lines-face nil))
      (occur "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s"))
    (let ((window (get-buffer-window "*Occur*")))
      (if window
          (select-window window)
        (switch-to-buffer "*Occur*")))))

;; sudo pacman -Syu pyright
(use-package lsp-pyright
  :hook (python-ts-mode . lsp-deferred))

;; sudo pacman -Syu python-black
(use-package python-black
  :hook (python-ts-mode . python-black-on-save-mode))

;; sudo pacman -Syu python-isort
(use-package python-isort
  :hook (python-ts-mode . python-isort-on-save-mode))

;; sudo pacman -Syu python-poetry
(use-package poetry
  :hook (python-ts-mode . (lambda () (local-set-key (kbd "C-c p") 'poetry))))

(use-package pippel)

;;;;;;;;;;;;;;;;;;;;;;;;;; C/C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sudo pacman -Syu clang
;; pipx install compiledb
(use-package cc-mode
  :ensure nil
  :hook
  (((c-mode c++-mode objc-mode cuda-mode) . lsp-deferred)
   ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (c-toggle-comment-style -1)))
   ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                              (add-hook 'before-save-hook
                                                        'lsp-format-buffer t))))
  :bind (:map c-mode-base-map
              ("C-c C-c" . ff-find-other-file))
  :config
  (advice-add 'c-update-modeline :around 'ignore))

(use-package cuda-mode)

(use-package modern-cpp-font-lock
  :hook (( c++-mode cuda-mode) . modern-c++-font-lock-mode))

;; pipx install cmake-language-server
(use-package cmake-ts-mode
  :init (ensure-treesit '(cmake "https://github.com/uyha/tree-sitter-cmake"))
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook ((cmake-ts-mode . lsp-deferred)
         (cmake-ts-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer t)))))

(use-package glsl-mode
  :bind (:map glsl-mode-map
              ("C-c C-c" . ff-find-other-file)
              ("C-?"     . glsl-find-man-page)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Godot ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pipx install gdtoolkit
(use-package gdscript-mode
  :init (ensure-treesit '(gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript"))
  :mode ("\\.gd\\'" . gdscript-ts-mode)
  :hook (gdscript-ts-mode . lsp-deferred)
  :bind (:map gdscript-comint--mode-map
              ("q" . (lambda ()
                       (interactive)
                       (let ((kill-buffer-query-functions nil))
                         (kill-current-buffer))))
              ("k" . kill-current-buffer))
  :config
  (setq lsp-gdscript-port 6008
        warning-suppress-types '((lsp-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Rust ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rustup toolchain install stable
;; rustup component add rust-src rust-analyzer
;; sudo pacman -Syu cargo-edit cargo-outdated cargo-msrv
;; sudo pacman -Syu sccache lld
(use-package rust-ts-mode
  :init (ensure-treesit '(rust "https://github.com/tree-sitter/tree-sitter-rust"))
  :mode "\\.rs\\'"
  :bind (:map rust-ts-mode-map
              ("C-c C-o" . rust-occur-definitions)
              ("C-c C-d" . lsp-rust-analyzer-open-external-docs)
              ("C-c TAB" . lsp-rust-analyzer-expand-macro))
  :hook
  ((rust-ts-mode . lsp-deferred)
   (rust-ts-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer t)))
   (rust-ts-mode . subword-mode))
  :config
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
  :after rust-ts-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package rust-mode
  :no-require t
  :hook (rust-ts-mode . (lambda () (require 'rust-compile))))

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode)
  :bind (:map cargo-minor-mode-command-map
              ("C-S-t" . lsp-rust-analyzer-related-tests)))

(use-package ron-mode)

;; cargo install pesta pest_fmt
(use-package pest-mode
  :hook (pest-mode . flymake-mode))

(use-package poporg
  :after rust-ts-mode
  :bind (:map rust-ts-mode-map ("C-c \"" . poporg-dwim))
  :init
  (setq rustdoc-attributes '("ignore" "should_panic" "no_run" "compile_fail" "edition2018"))
  :config
  (setq poporg-edit-hook '(gfm-mode)
        poporg-comment-skip-regexp "[[:space:]!/]*"
        markdown-fontify-code-block-default-mode 'rust-ts-mode)
  (defun markdown-get-lang-rust-mode (lang)
    (if (member lang rustdoc-attributes) 'rust-mode))
  (advice-add 'markdown-get-lang-mode :before-until 'markdown-get-lang-rust-mode))

;; pamac install wgsl-analyzer
(use-package wgsl-mode
  :hook
  ((wgsl-mode . lsp-deferred)
   (wgsl-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer t))))
  :config
  (setq c-basic-offset 4
        c-offsets-alist ;; set with C-c C-o
        '((label         . +)
          (arglist-intro . +)
          (arglist-close . -))))

;;; init.el ends here
