;;; paredit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "paredit" "../../../../.emacs.d/elpa/paredit-20160324.1215/paredit.el"
;;;;;;  "6f023f909dfdd92c8f573f17873141d1")
;;; Generated autoloads from ../../../../.emacs.d/elpa/paredit-20160324.1215/paredit.el

(autoload 'paredit-mode "paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  unbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are unbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing unbalanced parentheses instead.
\\<paredit-mode-map>

\(fn &optional ARG)" t nil)

(autoload 'enable-paredit-mode "paredit" "\
Turn on pseudo-structural editing of Lisp code.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/paredit-20160324.1215/paredit-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/paredit-20160324.1215/paredit.el")
;;;;;;  (22292 64411 783015 871000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; paredit-autoloads.el ends here
