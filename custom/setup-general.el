;;; setup-general.el --- general config
;;; commentary:

;;; Code:
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; (load-theme 'tango-dark)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key "\C-x2" (lambda() (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda() (interactive)(split-window-horizontally) (other-window 1)))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Make frame transparency overridable
(defvar chwang/frame-transparency '(90 . 90))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha chwang/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,chwang/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (use-package command-log-mode)

(setq gc-cons-threshold 100000000)

(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
;;(setq-default indent-tabs-mode t)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
(windmove-default-keybindings)

(use-package sr-speedbar)

(provide 'setup-general)
;;; setup-general.el ends here
