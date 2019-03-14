;;; init.el --- fs emacs init.el

;;; Commentary:
;; run Emacs with arg --fs-init first time
;; setup flow: init.el => if fs-init then setup.sh; setup.el; else setup.el.
;; 

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(package-initialize)
(add-to-list 'load-path
	     (substitute-in-file-name "$HOME/.emacs.d/fs"))


;; Setup emacs dependencies

(defvar fs-init-buffer nil)


(defun fs-init-sentinel (process event)
  "Sentinel for fs-init procedure, PROCESS EVENT."
  (if (equal event "finished\n")
      (progn
	(kill-buffer fs-init-buffer)
	(require 'setup))
    (message "fs-init error, @event: %s" event)))

(defun fs-init-setup ()
  "Init setup."
  (interactive)
  (let (
         (fs-init-sh-cmd
          (concat "echo "
                  (read-passwd "sudo password: ")
                  " | sudo -S bash "
                  (substitute-in-file-name "$HOME/.emacs.d/fs/setup.sh"))))
    (setq fs-init-buffer (get-buffer-create "fs-init"))
    (set-process-sentinel
     (start-process-shell-command "fs-init" fs-init-buffer fs-init-sh-cmd)
     'fs-init-sentinel)
    (display-buffer fs-init-buffer)))

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
	(when (and fs-opt-cb (member fs-opt-name command-line-args))
	    ;; if callback is not nill and a opt is given
	      ;; delete from command-line-args
	      (delete fs-opt-name command-line-args)
	      (funcall fs-opt-cb)))
      (setq idx (1+ idx)))))

(fs-init-fs-option)

(unless fs-init-buffer
  (require 'setup))

(provide 'init)

;;; init.el ends here
