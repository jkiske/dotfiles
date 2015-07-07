;; -*- mode: Emacs-Lisp;-*-

;; Turn on debugging, it will be turned off at the end. In case something
;; happens during loading that breaks something, it's nice to have a debug
;; information.
(setq debug-on-error t)
(setq debug-on-quit t)

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

(defun byte-recompile-init-files ()
  "Recompile all of the startup files"
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

;; Turn off debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
