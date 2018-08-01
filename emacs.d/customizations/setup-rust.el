;;;;
;; Rust
;;;;

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "C-c TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
