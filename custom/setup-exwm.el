;;; setup-exwm --- exwm config
;;; Commentary:

;;; Code:
(defun chwang/run-in-background (command)
  "COMMAND."
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun chwang/exwm-init-hook ()
  "Make workspace 1 be the one where we land at startup."
  ;;(exwm-workspace-switch-create 1)

  ;; Open multi-term by default
  (use-package multi-term)
  (setq multi-term-program "/bin/bash")
  (multi-term)

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; Show the time and date in mode line
  (setq display-time-day-and-date t)
  (defvar display-time-format)
  (setq display-time-format "%F:%R")
  (display-time-mode 1)

  ;; Launch apps that will run in the background
  (chwang/run-in-background "dunst")
  (chwang/run-in-background "nm-applet")
  (chwang/run-in-background "pasystray")
  (chwang/run-in-background "blueman-applet")
  )

(defun chwang/exwm-update-class ()
  "Make the workspace to display the name of its class."
  (defvar exwm-class-name)
  (exwm-workspace-rename-buffer exwm-class-name))

(require 'exwm-workspace)

(defun chwang/exwm-update-title ()
  "Update workspace buffer title."
  (defvar exwm-class-name)
  (defvar exwm-title)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun chwang/set-wallpaper ()
  "Set wallpaper."
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /usr/share/backgrounds/Focal-Fossa_WP_4096x2304_GREY.png"))

(defun chwang/configure-window-by-class ()
  "Configure window per class name."
  (interactive)
  ;;(message "Window '%s' appeared!" exwm-class-name)
  (defvar exwm-class-name)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 2)
     ;;(exwm-floating-toggle-floating)
     ;;(exwm-layout-toggle-mode-line)
     )))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 8)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'chwang/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'chwang/exwm-update-title)

  ;; When EXWM starts up, do some extra configuration
  (add-hook 'exwm-init-hook #'chwang/exwm-init-hook)

  ;; Configure window as they're created
  ;;(add-hook 'exwm-manage-finish-hook #'chwang/configure-window-by-class)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set the screen resolution
  (require 'exwm-randr)
  (declare-function exwm-randr-enable "exwm-randr" ())
  (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual1 --primary --scale 0.66x0.66 -xos 0x0 --rotate normal --output Virtual2 --off --output Virtual3 --off --output Virtual4 --off --output Virtual5 --off --output Virtual6 --off --output Virtual7 --off --output Virtual8 --off")

  (chwang/set-wallpaper)

  (require 'exwm-systemtray)
  (defvar exwm-systemtray-height 24)
  (declare-function exwm-systemtray-enable "exwm-systemtray" ())
  (exwm-systemtray-enable)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

  (exwm-enable))

(provide 'setup-exwm)
;;; setup-exwm.el ends here
