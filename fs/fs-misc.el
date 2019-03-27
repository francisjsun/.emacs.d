;;; fs-misc.el --- misc staff

;;; Commentary:
;; 

;;; Code:

;; indentation setup
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
;; adding flycheck to elisp-mode
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (flycheck-mode)))

;; cursor setup
(add-hook 'prog-mode-hook (lambda ()
			    (setq cursor-type 'hbar)
			    (setq blink-cursor-blinks 0)
			    (setq x-stretch-cursor t)))

;; disable startup message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)

;; disable menu tool scroll bar
(menu-bar-mode -1)
(when window-system
      (tool-bar-mode -1)
      (scroll-bar-mode -1))

;; linum mode
(global-linum-mode t)

;; smartparents

(require 'smartparens-config)
(smartparens-global-mode)

;; ace-window
(global-set-key (kbd "C-o") 'ace-window)

;; color
(set-background-color "black")
(set-foreground-color "white")

(setq frame-background-mode 'dark)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(rainbow-delimiters-depth-1-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(rainbow-delimiters-mismatched-face ((t (:foreground "red"))))
 )

;; tweak env in OS X
(when (eq window-system 'ns)
  (if (fboundp 'exec-path-from-shell-initialize)
      (exec-path-from-shell-initialize)
    (message "exec-path-from-shell not install?")))

;; carriage return character setup
(prefer-coding-system 'utf-8-unix)

;; ext of dired
(defun fs-dired-indent-marked-files()
  "Do \"indent-region\" foreach file in marked-files."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

;; elpy setup
(require 'elpy)
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(pyvenv-workon "elpy-venv")
(pyvenv-activate "~/.emacs.d/elpy-venv")
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(provide 'fs-misc)

;;; fs-misc.el ends here
