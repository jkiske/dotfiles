;; -*- mode: Emacs-Lisp;-*-

;; Turn on debugging, it will be turned off at the end. In case something
;; happens during loading that breaks something, it's nice to have a debug
;; information.
(setq debug-on-error t)
(setq debug-on-quit t)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Install packages
(defvar local-packages '(use-package))
(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p) nil p)) packages)))
(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn (package-refresh-contents)
           (dolist (p need-to-install)
             (package-install p)))))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Load common files
(load "~/.emacs_common")

;; When we are on a mac
(when (eq system-type 'darwin)
  (load "~/.emacs_mac"))

;; When we are using a GUI
(when (display-graphic-p)
  (load "~/.emacs_gui"))

;; Keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Turn off debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
