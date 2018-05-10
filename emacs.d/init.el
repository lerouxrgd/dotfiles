;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("gnu"          . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa"        . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-milk"   . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("tromey"       . "http://tromey.com/elpa/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Define packages to install
(defvar my-packages
  '(;; Makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; Colorful parenthesis matching
    rainbow-delimiters

    ;; Integration with Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    ;; https://github.com/clojure-emacs/cider
    ;; https://github.com/clojure-emacs/clj-refactor.el
    clojure-mode
    clojure-mode-extra-font-locking    
    cider
    clj-refactor

    ;; Integration with Python
    ;; https://github.com/jorgenschaefer/elpy
    elpy
    ;; pip install rope
    ;; pip install flake8
    ;; pip install importmagic
    ;; pip install autopep8
    ;; pip install yapf

    ;; Integration with Go
    ;; https://github.com/dominikh/go-mode.el
    ;; https://github.com/dougm/goflymake
    ;; https://github.com/rogpeppe/godef
    ;; https://github.com/nsf/gocode#emacs-setup
    go-mode
    go-guru
    go-autocomplete
    ;; go get -u golang.org/x/tools/cmd/...
    ;; go get -u github.com/dougm/goflymake
    ;; go get -u github.com/rogpeppe/godef/...
    ;; go get -u github.com/nsf/gocode

    ;; Integration with Javascript
    ;; https://github.com/mooz/js2-mode
    ;; https://github.com/magnars/js2-refactor.el
    ;; https://github.com/nicolaspetton/xref-js2
    ;; https://github.com/ggreer/the_silver_searcher
    ;; https://github.com/ternjs/tern
    js2-mode
    js2-refactor
    xref-js2
    company-tern
    ;; sudo dnf install the_silver_searcher
    ;; sudo npm install -g tern

    ;; Validation for JSON
    ;; https://github.com/purcell/flymake-json
    json-mode
    flymake-json
    ;; sudo npm install jsonlint -g

    ;; Edit html tags like sexps
    tagedit

    ;; Integration with Markdown
    ;; https://github.com/jrblevin/markdown-mode
    markdown-mode

    ;; Integration with Lua
    ;; http://immerrr.github.io/lua-mode/
    lua-mode

    ;; Integration with Octave
    ;; https://github.com/coldnew/ac-octave
    ac-octave

    ;; Integration with Git
    ;; https://github.com/magit/magit
    magit

    ;; Allows ido usage in as many contexts as possible.
    ;; See customizations/navigation.el for a description of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands.
    ;; Provides a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; Displays the key bindings following current incomplete command
    ;; https://github.com/justbur/emacs-which-key
    which-key

    ;; project navigation
    projectile
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-python.el")
(load "setup-go.el")
(load "setup-js2.el")
(load "setup-lua.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-octave tagedit smex rainbow-delimiters projectile paredit lua-mode ido-ubiquitous clojure-mode-extra-font-locking cider))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
