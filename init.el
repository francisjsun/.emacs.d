;;; init.el --- fs emacs init.el

;;; Commentary:
;; run Emacs with arg --fs-init when first time for dependencies setup
;; 

;;; Code:

;; Setup emacs dependencies

;; run sudo command
(defun fs-sudo-shell-command (cmd)
  "Run sudo command.
Argument CMD shell cmd"
  (interactive "scmd:")
  (shell-command (concat "echo "
			 (read-passwd "sudo password:")
			 " | sudo -S " cmd)
		 "fs-sudo-shell-command-output"))

(defun fs-init-setup ()
  "Init setup."
  (fs-sudo-shell-command
   (substitute-in-file-name "$HOME/.emacs.d/fs/setup.sh")))

;; removed because this hook function will be called after init.el done,
;; and this is not what I want
;; (setq command-switch-alist
;;       '(("-fs-init" . fs-init-setup)))

(defconst fs-init-fs-option-alist
  '(
    ("--fs-init" fs-init-setup)
    )
  "My custom command line args option.")


(defun fs-init-fs-option ()
  "Processing --fs-* custom options."
  (let ((idx 0)
	(lstLen (safe-length fs-init-fs-option-alist)))
    ;; iterate through fs-init-fs-option-alist
    (while (< idx lstLen)
      (let* ((fs-opt (nth idx fs-init-fs-option-alist))
	     (fs-opt-name (car fs-opt))
	     (fs-opt-cb (car-safe (cdr-safe fs-opt))))
	(if (and fs-opt-cb (member fs-opt-name command-line-args))
	    ;; if callback is not nill and a opt is given
	    (progn
	      ;; delete from command-line-args
	      (delete fs-opt-name command-line-args)
	      (funcall fs-opt-cb))))
      (setq idx (1+ idx)))))

(fs-init-fs-option)

;;; normal init.el begins here
(add-to-list 'load-path
	     (substitute-in-file-name "$HOME/.emacs.d/fs"))

(let (
      ;; optimizing for setup
      ;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
      (file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      )
  (require 'fs-package)
  (require 'fs-misc)
  (require 'fs-company)
  (require 'fs-cc-mode)
  (require 'fs-flycheck)
  )

;;;; optimization

;; GC threshold
(defconst fs-init-gc-cons-threshold-default
  gc-cons-threshold
  "Default GC threashold value.")

;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold
(defun fs-init-gc-cons-threshold-tweaks (ismax)
  "Argument ISMAX if going to set gc-cons-threashold to max."
  (if ismax
      (setq gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-threshold fs-init-gc-cons-threshold-default)
    ))

(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (fs-init-gc-cons-threshold-tweaks t)))

(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (fs-init-gc-cons-threshold-tweaks nil)))

;;(emacs-init-time)

;;; init.el ends here
