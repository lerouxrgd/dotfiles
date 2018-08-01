;;;;
;; Golang
;;;;

(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))
(require 'go-flymake)
;;(require 'go-flycheck)

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (go-guru-hl-identifier-mode)                    ; highlight identifiers
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default))

(require 'go-guru)
