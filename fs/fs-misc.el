;;; fs-misc.el --- misc staff

;;; Commentary:
;; 

;;; Code:

;; disable startup message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)

;; disable menu tool scroll bar
(menu-bar-mode -1)
(if window-system
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      ))

;; linum mode
(global-linum-mode t)

;; smartparents

(require 'smartparens-config)
(smartparens-global-mode)

;; ace-window
(global-set-key (kbd "M-p") 'ace-window)

;; color
(set-background-color "black")
(set-foreground-color "white")

;; tweak env in OS X
(if (eq window-system 'ns)
    (exec-path-from-shell-initialize))

(provide 'fs-misc)

;;; fs-misc.el ends here
