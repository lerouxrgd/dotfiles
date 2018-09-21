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

;; Define packages to install
(setq package-selected-packages
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
    ;; sudo pacman -Syu ipython
    ;; pip install --user rope
    ;; pip install --user flake8
    ;; pip install --user importmagic
    ;; pip install --user autopep8
    ;; pip install --user yapf

    ;; Integration with Go
    ;; https://github.com/dominikh/go-mode.el
    ;; https://github.com/rogpeppe/godef
    ;; https://github.com/nsf/gocode#emacs-setup
    go-mode
    go-guru
    go-autocomplete
    ;; go get -u golang.org/x/tools/cmd/...
    ;; go get -u github.com/rogpeppe/godef/...
    ;; go get -u github.com/nsf/gocode

    ;; Integration with Rust
    ;; https://github.com/rust-lang/rust-mode
    ;; https://github.com/kwrooijen/cargo.el
    ;; https://github.com/racer-rust/emacs-racer
    ;; https://github.com/dryman/toml-mode.el
    ;; https://github.com/flycheck/flycheck-rust
    rust-mode
    cargo
    racer
    toml-mode
    flycheck-rust
    ;; rustup component add rustfmt-preview
    ;; rustup component add rust-src
    ;; rustup toolchain add nightly
    ;; cargo +nightly install racer

    ;; Integration with Lua
    ;; http://immerrr.github.io/lua-mode/
    lua-mode

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
    ;; sudo pacman -Syu the_silver_searcher
    ;; sudo npm install -g tern

    ;; Integration with JSON
    ;; https://github.com/DamienCassou/json-navigator
    json-mode
    json-navigator

    ;; Integration with HTML
    ;; Edit html tags like sexps
    tagedit

    ;; Integration with Markdown (and live preview)
    ;; https://github.com/jrblevin/markdown-mode
    ;; https://github.com/mola-T/flymd
    markdown-mode
    flymd

    ;; Integration with YAML
    ;; https://github.com/yoshiki/yaml-mode
    yaml-mode

    ;; Integration with Docker
    ;; https://github.com/spotify/dockerfile-mode
    dockerfile-mode

    ;; Integration with Git
    ;; https://github.com/magit/magit
    magit

    ;; Syntax checkers
    ;; https://github.com/flycheck/flycheck
    flycheck
    ;; sudo npm install jsonlint -g
    ;; sudo npm install js-yaml -g

    ;; Text completion
    ;; https://github.com/company-mode/company-mode
    company
    
    ;; Allows ido usage in as many contexts as possible.
    ;; See customizations/navigation.el for a description of ido
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
    ido-completing-read+

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

(defun install-packages ()
  "Install/refresh all required packages, when needed."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; Place downloaded elisp files in ~/.emacs.d/vendor.
;; You'll then be able to load them.
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Activate syntax checkers
(global-flycheck-mode)

;; Activate key autocompletion
(which-key-mode)

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things below,
;; Emacs knows where to look for the corresponding file.
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

;; These customizations make editing a bit nicer
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-go.el")
(load "setup-python.el")
(load "setup-rust.el")
(load "setup-lua.el")
(load "setup-js2.el")
(load "setup-yaml.el")
