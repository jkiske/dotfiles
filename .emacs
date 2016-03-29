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

(setq config "~/.emacs")
(setq config_common "~/.emacs_common")
(setq config_mac "~/.emacs_mac")
(setq config_gui "~/.emacs_gui")

;; Reload the config file
(global-set-key (kbd "C-c e r")
                (lambda () (interactive) (load-file config)))

;; Open up config files
(global-set-key (kbd "C-c e e")
                (lambda () (interactive) (find-file config)))
(global-set-key (kbd "C-c e c")
                (lambda () (interactive) (find-file config_common)))
(global-set-key (kbd "C-c e m")
                (lambda () (interactive) (find-file config_mac)))
(global-set-key (kbd "C-c e g")
                (lambda () (interactive) (find-file config_gui)))

(defun byte-recompile-init-files ()
  "Recompile all of the startup files"
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

;; Load common files
(load config_common)

;; When we are on a mac
(when (eq system-type 'darwin)
  (load config_mac))

;; When we are using a GUI
(when (display-graphic-p)
  (load config_gui))

;; Keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Turn off debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
