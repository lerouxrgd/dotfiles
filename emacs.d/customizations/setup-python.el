;;;;
;; Python
;;;;

(elpy-enable)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(setq ac-modes (delq 'python-mode ac-modes))
