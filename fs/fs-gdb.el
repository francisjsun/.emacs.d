;;; fs-gdb.el --- gud-gdb setup
;;gud-gdb

;;; Commentary:
;; 


;;; Code:

(require 'gdb-mi)
(setq gdb-many-windows t)

;;recover window layout from gud-gdb
(defconst fs-gdb-window-config-register
  "fs-gdb-window-layout-register")
(defun fs-gdb-set-my-current-windowlayout ()
  "Save current windows layout."
  (window-configuration-to-register fs-gdb-window-config-register))

(defun fs-gdb-start ()
  "Save current windows layout and call gdb."
  (interactive)
  (fs-gdb-set-my-current-windowlayout)
  (call-interactively 'gdb))

(defun fs-gdb-quit()
  "Quit gdb and recover previous windows layout"
  (interactive)
  (gud-basic-call "quit")
  (jump-to-register fs-gdb-window-config-register))

(add-hook 'gud-mode-hook (lambda ()
			   (local-unset-key (kbd "C-q"))
			   (local-set-key (kbd "C-q") 'fs-gdb-quit)))


(provide 'fs-gdb)

;;; fs-gdb.el ends here
