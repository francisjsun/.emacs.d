(provide 'fs-misc)

;; disable startup message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)

;; linum mode
(global-linum-mode t)

;; smartparents
(require 'smartparens-config)
(smartparens-global-mode)

;; color
(set-background-color "black")
(set-foreground-color "white")


