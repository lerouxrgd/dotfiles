;;; cider-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cider" "../../../../.emacs.d/elpa/cider-20160417.1146/cider.el"
;;;;;;  "ebe282ec5eb76036fa55d109dcc62a0c")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider.el

(autoload 'cider-version "cider" "\
Display CIDER's version.

\(fn)" t nil)

(autoload 'cider-jack-in "cider" "\
Start an nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.
If CLJS-TOO is non-nil, also start a ClojureScript REPL session with its
own buffer.

\(fn &optional PROMPT-PROJECT CLJS-TOO)" t nil)

(autoload 'cider-jack-in-clojurescript "cider" "\
Start an nREPL server and connect to it both Clojure and ClojureScript REPLs.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(autoload 'cider-connect "cider" "\
Connect to an nREPL server identified by HOST and PORT.
Create REPL buffer and start an nREPL client connection.

When the optional param PROJECT-DIR is present, the connection
gets associated with it.

\(fn HOST PORT &optional PROJECT-DIR)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in) (define-key clojure-mode-map (kbd "C-c M-J") #'cider-jack-in-clojurescript) (define-key clojure-mode-map (kbd "C-c M-c") #'cider-connect)))

;;;***

;;;### (autoloads nil "cider-apropos" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-apropos.el"
;;;;;;  "ec13ad8ac5e0f7a5b5e984b88eb20882")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-apropos.el

(autoload 'cider-apropos "cider-apropos" "\
Show all symbols whose names match QUERY, a regular expression.
QUERY can also be a list of space-separated words (e.g. take while) which
will be converted to a regular expression (like take.+while) automatically
behind the scenes.  The search may be limited to the namespace NS, and may
optionally search doc strings (based on DOCS-P), include private vars
\(based on PRIVATES-P), and be case-sensitive (based on CASE-SENSITIVE-P).

\(fn QUERY &optional NS DOCS-P PRIVATES-P CASE-SENSITIVE-P)" t nil)

(autoload 'cider-apropos-documentation "cider-apropos" "\
Shortcut for (cider-apropos <query> nil t).

\(fn)" t nil)

(autoload 'cider-apropos-select "cider-apropos" "\
Similar to `cider-apropos', but presents the results in a completing read.

Show all symbols whose names match QUERY, a regular expression.
QUERY can also be a list of space-separated words (e.g. take while) which
will be converted to a regular expression (like take.+while) automatically
behind the scenes.  The search may be limited to the namespace NS, and may
optionally search doc strings (based on DOCS-P), include private vars
\(based on PRIVATES-P), and be case-sensitive (based on CASE-SENSITIVE-P).

\(fn QUERY &optional NS DOCS-P PRIVATES-P CASE-SENSITIVE-P)" t nil)

(autoload 'cider-apropos-documentation-select "cider-apropos" "\
Shortcut for (cider-apropos-select <query> nil t).

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-browse-ns" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-browse-ns.el"
;;;;;;  "46d8323b41eccdeeeaca90034d5d4e28")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-browse-ns.el

(autoload 'cider-browse-ns "cider-browse-ns" "\
List all NAMESPACE's vars in BUFFER.

\(fn NAMESPACE)" t nil)

(autoload 'cider-browse-ns-all "cider-browse-ns" "\
List all loaded namespaces in BUFFER.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-classpath" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-classpath.el"
;;;;;;  "aa4d87e81b7a1d0ddb0197a670725bb9")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-classpath.el

(autoload 'cider-classpath "cider-classpath" "\
List all classpath entries.

\(fn)" t nil)

(autoload 'cider-open-classpath-entry "cider-classpath" "\
Open a classpath entry.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-debug" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-debug.el"
;;;;;;  "316796ae178ece3635789bb1ba7a897f")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-debug.el

(autoload 'cider-debug-defun-at-point "cider-debug" "\
Instrument the \"top-level\" expression at point.
If it is a defn, dispatch the instrumented definition.  Otherwise,
immediately evaluate the instrumented expression.

While debugged code is being evaluated, the user is taken through the
source code and displayed the value of various expressions.  At each step,
a number of keys will be prompted to the user.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-grimoire" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-grimoire.el"
;;;;;;  "69e9c8eb53c0a2ec63455edd4c7ab8dd")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-grimoire.el

(autoload 'cider-grimoire-web "cider-grimoire" "\
Open grimoire documentation in the default web browser.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

(autoload 'cider-grimoire "cider-grimoire" "\
Open grimoire documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider-inspector" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-inspector.el"
;;;;;;  "9ccf122adeef76e0df8a85c894ed67f6")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-inspector.el

(autoload 'cider-inspect-last-sexp "cider-inspector" "\
Inspect the result of the the expression preceding point.

\(fn)" t nil)

(autoload 'cider-inspect-defun-at-point "cider-inspector" "\
Inspect the result of the \"top-level\" expression at point.

\(fn)" t nil)

(autoload 'cider-inspect-read-and-inspect "cider-inspector" "\
Read an expression from the minibuffer and inspect its result.

\(fn)" t nil)

(autoload 'cider-inspect "cider-inspector" "\
Inspect the result of the preceding sexp.

With a prefix argument ARG it inspects the result of the \"top-level\" form.
With a second prefix argument it prompts for an expression to eval and inspect.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider-macroexpansion" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-macroexpansion.el"
;;;;;;  "c7ba028161d6845c6af10bcab9b392d6")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-macroexpansion.el

(autoload 'cider-macroexpand-1 "cider-macroexpansion" "\
Invoke \\=`macroexpand-1\\=` on the expression preceding point.
If invoked with a PREFIX argument, use \\=`macroexpand\\=` instead of
\\=`macroexpand-1\\=`.

\(fn &optional PREFIX)" t nil)

(autoload 'cider-macroexpand-all "cider-macroexpansion" "\
Invoke \\=`clojure.walk/macroexpand-all\\=` on the expression preceding point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-mode" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-mode.el"
;;;;;;  "0e64b669b6369c52183ea13461ad21b9")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-mode.el

(defvar cider-mode-line '(:eval (format " cider[%s]" (cider--modeline-info))) "\
Mode line lighter for `cider-mode'.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how `cider-mode' displays its
status in the mode line.  The default value displays the current connection.
Set this variable to nil to disable the mode line
entirely.")

(custom-autoload 'cider-mode-line "cider-mode" t)

(eval-after-load 'clojure-mode '(easy-menu-define cider-clojure-mode-menu-open clojure-mode-map "Menu for Clojure mode.\n  This is displayed in `clojure-mode' buffers, if `cider-mode' is not active." `("CIDER" :visible (not cider-mode) ["Start a REPL" cider-jack-in :help "Starts an nREPL server (with lein, boot, or maven) and connects a REPL to it."] ["Connect to a REPL" cider-connect :help "Connects to a REPL that's already running."] ["Start a Clojure REPL, and a ClojureScript REPL" cider-jack-in-clojurescript :help "Starts an nREPL server, connects a Clojure REPL to it, and then a ClojureScript REPL.\n  Configure `cider-cljs-lein-repl' to change the ClojureScript REPL to use."] "--" ["View manual online" cider-view-manual])))

(autoload 'cider-mode "cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider-scratch" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-scratch.el"
;;;;;;  "a76ef172e9b384e53f0b809becec994e")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-scratch.el

(autoload 'cider-scratch "cider-scratch" "\
Go to the scratch buffer named `cider-scratch-buffer-name'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-selector" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-selector.el"
;;;;;;  "27c35baef4e91186c36ea1869e214ff2")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-selector.el

(autoload 'cider-selector "cider-selector" "\
Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer.  The `?' character describes then
available methods.  OTHER-WINDOW provides an optional target.

See `def-cider-selector-method' for defining new methods.

\(fn &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads nil "cider-test" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-test.el"
;;;;;;  "1c2e8d13c36c766369c1050e50efb323")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-test.el

(defvar cider-auto-test-mode nil "\
Non-nil if Cider-Auto-Test mode is enabled.
See the command `cider-auto-test-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `cider-auto-test-mode'.")

(custom-autoload 'cider-auto-test-mode "cider-test" nil)

(autoload 'cider-auto-test-mode "cider-test" "\
Toggle automatic testing of Clojure files.

When enabled this reruns tests every time a Clojure file is loaded.
Only runs tests corresponding to the loaded file's namespace and does
nothing if no tests are defined or if the file failed to load.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider-util" "../../../../.emacs.d/elpa/cider-20160417.1146/cider-util.el"
;;;;;;  "debf78159c378e68ab077ffdee134d54")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cider-20160417.1146/cider-util.el

(autoload 'cider-view-manual "cider-util" "\
View the manual in your default browser.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/cider-20160417.1146/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-client.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-common.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-compat.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-debug.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-doc.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-eldoc.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-interaction.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-overlays.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-popup.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-repl.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-resolve.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-selector.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-stacktrace.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-test.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider-util.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/cider.el"
;;;;;;  "../../../../.emacs.d/elpa/cider-20160417.1146/nrepl-client.el")
;;;;;;  (22292 64427 852100 937000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cider-autoloads.el ends here
