(provide 'fs-cc-mode)

(require 'cc-mode)

;; coding style
(defun fs-cc-mode-coding-style ()
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (setq c-default-style "linux"
	c-basic-offset 4
	)
  )

;; code complete
(defun fs-cc-mode-code-complete ()
  (setq company-clang-arguments
	'(
	  "-I/usr/local"
	  "-std=c++11"
	  )
	)
  )
;; ff-find-other-file
(defun fs-cc-mode-ff-f-o-f ()
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  )

;; misc
(defun fs-cc-mode-misc ()
  )

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (fs-cc-mode-coding-style)
	    (fs-cc-mode-ff-f-o-f)
	    (fs-cc-mode-misc)))



