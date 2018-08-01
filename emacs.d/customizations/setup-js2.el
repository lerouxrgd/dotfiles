;;;;
;; Javascript
;;;;

(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)

(require 'company)
(require 'company-tern)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js-indent-level 2)

;; js-mode (which js2 is based on) binds "M-."
;; which conflicts with xref, so unbind it.
(define-key js-mode-map (kbd "M-.") nil)
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(js2r-add-keybindings-with-prefix "C-c C-r")

(add-to-list 'company-backends 'company-tern)

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
                           
;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;;;;
;; HTML
;;;;

(add-hook 'html-mode-hook 'subword-mode)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

;;;;
;; Markdown
;;;;

(require 'flymd)

(setq flymd-output-directory "/tmp/flymd")
(setq flymd-close-buffer-delete-temp-files t)

;;;;
;; JSON
;;;;

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))
