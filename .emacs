(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Install packages
(defvar local-packages '(auto-complete
                         auto-dim-other-buffers
                         flx-ido
                         git-gutter+
                         magit
                         multi-web-mode
                         multiple-cursors
                         paren
                         pbcopy
                         popup
                         popup-kill-ring
                         pos-tip
                         powerline
                         redo+
                         smex
                         zenburn-theme
                         exec-path-from-shell
                         ))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p) nil p)) packages)))

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
        (package-install p)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  ;; (setq mac-command-modifier 'super)
  (global-set-key [kp-delete] 'delete-char) ;; Sets fn-delete to be right-delete
  (set-face-attribute 'default nil :family "Monaco")

  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; No menu-bar
(menu-bar-mode -1)
;; Save the desktop
(desktop-save-mode 1)
;; Always show column numbers
(column-number-mode t)
;; Line numbers
(global-set-key (kbd "C-x C-n") 'global-linum-mode)
;; Zenburn
(load-theme 'zenburn t)
;; Make garbage collector happen ever 20MB allocated
(setq gc-cons-threshold 20000000)
;; Deletes selected text
(delete-selection-mode t)
;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; Autowraps after 79 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 79)
;; Clear all whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Replaces white space with just one space
(global-set-key (kbd "C-c C-SPC") 'just-one-space)
;; Don't ding
(setq ring-bell-function 'ignore)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Autosave files go to a temp folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(set-cursor-color "salmon1")
;; Disable line wrapping in minibuffer
(add-hook 'minibuffer-setup-hook
  (lambda () (setq truncate-lines nil)))
;; When we are using a GUI
(when (display-graphic-p)
  (setq-default cursor-type 'bar)
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (global-unset-key "\C-z"))

(defun describe-last-function()
  (interactive)
  (describe-function last-command))

;; Makes OSX Load the path from the shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Adds git changes to the gutter on emacs
(require 'git-gutter+)
(global-git-gutter+-mode t)
(global-set-key (kbd "M-g M-p") 'git-gutter+-previous-hunk)
(global-set-key (kbd "M-g M-n") 'git-gutter+-next-hunk)

;; Fuzzy search for file names
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Simple autocomplete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete  t)

;; Python auto-complete
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method '(pos-tip))
(defun my/jedi-mode-hook ()
  (set (make-local-variable 'ac-max-width) 0.5)
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-/") 'jedi:show-doc)
  (local-set-key (kbd "M-?") 'jedi:get-in-function-call))
(add-hook 'jedi-mode-hook 'my/jedi-mode-hook)

;; Setup multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c s") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c S") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c n") 'mc/mark-next-symbol-like-this)

(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(set-face-attribute 'show-paren-match nil :underline t)
(set-face-background 'show-paren-mismatch (face-background 'default))
(set-face-foreground 'show-paren-mismatch "red")
(set-face-attribute 'show-paren-mismatch nil :underline t)

(require 'magit)
(global-set-key (kbd "M-g M-s") 'magit-status)
(global-set-key (kbd "M-g M-c") 'magit-commit)
(global-set-key (kbd "M-g M-d") 'magit-diff-unstaged)
(setq magit-last-seen-setup-instructions "1.4.0")
;; Have emacs autoreload on git resets
(global-auto-revert-mode 1)

(require 'auto-dim-other-buffers)
(auto-dim-other-buffers-mode t)
(set-face-attribute 'auto-dim-other-buffers-face nil :background "gray15")

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; MacPorts specific
(setq ispell-program-name "/opt/local/bin/ispell")

;; https://github.com/jonathanchu/emacs-powerline
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(defvar powerline-color0)
(setq powerline-color0 "grey60")

(defpowerline display-time display-time-string)
(setq display-time-format "%I:%M%p | %a %D |")
(setq display-time-default-load-average nil)
(display-time-mode t)

(set-face-attribute 'mode-line nil
                    :foreground "#030303"
                    :background "#BFEBBF"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#f9f9f9"
                    :background powerline-color0
                    :box nil)

;; Special characters:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
(setq-default mode-line-format
  (list "%e"
    '(:eval (append
       (list
        (powerline-make-text       "| %2I | %* | %b "   nil)
        (powerline-arrow           'left                  nil  powerline-color1  )
        (powerline-make-text       " %c | %l (%p)"        powerline-color1  )
        (powerline-narrow          'left                  powerline-color1  powerline-color2  )
        (powerline-major-mode      'left                  powerline-color2  )
        (powerline-make-text       " | "                  powerline-color2  )
        (powerline-minor-modes     'center                powerline-color2  ))
     (powerline-pull-right
      (list
        (powerline-vc              'right                powerline-color1  powerline-color2  )
        (powerline-display-time    'right                powerline-color0  powerline-color1  )
        (powerline-make-text       "%-"                  powerline-color0  )))))))

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; "undo" (and "redo") changes in the window configuration with the key
;; commands "C-c left" and "C-c right"
(winner-mode 1)
(global-set-key (kbd "M-n") 'next-multiframe-window)
(global-set-key (kbd "M-p") 'previous-multiframe-window)

;; Shows entire kill ring history
(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)
(global-set-key (kbd "C-y") 'yank)
(global-set-key (kbd "M-y") 'popup-kill-ring)

(defun flycheck-python-setup ()
  (flycheck-mode))
(add-hook 'python-mode-hook #'flycheck-python-setup)

;; Undo is C-/, Redo is C-?
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)
