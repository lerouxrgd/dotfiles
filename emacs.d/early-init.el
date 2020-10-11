;;; early-init.el --- Emacs early initialization -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Startup GC tuning
(defvar tmp--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 1024 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist tmp--file-name-handler-alist)))

;; Remove unnecessary graphical elements
(scroll-bar-mode -1) ; Turn off native OS scroll bars
(tool-bar-mode   -1) ; Turn off tool bar
(menu-bar-mode   -1) ; Turn off menu bars
(tooltip-mode    -1) ; Turn off pop-up stuffs

;; As of Emacs 27 it is no longer necessary to call `package-initialize'
(setq package--initialized t)

;;; early-init.el ends here
