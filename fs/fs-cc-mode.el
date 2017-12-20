;;; fs-cc-mode.el --- c/c++ setup



;;; Commentary:
;; 

(require 'cc-mode)

;; should added before cc-mode enabled
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; coding style
;;; Code:

(defun fs-cc-mode-coding-style ()
  "Coding style setup."
  (setq c-default-style "linux"
	c-basic-offset 4
	))

;; clang-args
(defconst fs-cc-mode-clang-args
  '(
    "-I/usr/local"
    "-std=c++11")
  "Clang args for company-clang and flycheck."
  )

;; code complete
(require 'company-clang)
(defun fs-cc-mode-code-complete ()
  "Company clang args setup."
  (setq company-clang-arguments
	'fs-cc-mode-clang-args))


;; ff-find-other-file
(defun fs-cc-mode-ff-f-o-f ()
  "Find corresponding file setup."
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  )

;; flycheck setup
(require 'flycheck)
(defun fs-cc-mode-flycheck ()
  "Flycheck setup."
  (flycheck-select-checker 'c/c++-clang)
  (setq flycheck-clang-args
	fs-cc-mode-clang-args))

;; misc
;;(defun fs-cc-mode-misc ()
;  )

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (fs-cc-mode-coding-style)
	    (fs-cc-mode-ff-f-o-f)
	    (fs-cc-mode-flycheck)
	    ))

(provide 'fs-cc-mode)

;;; fs-cc-mode.el ends here
