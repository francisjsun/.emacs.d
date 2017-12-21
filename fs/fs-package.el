;;; fs-package.el --- install package iff not installed

;;; Commentary:
;; 


;;; Code:

(require 'package)

;; using emacs-china mirror
(setq package-archives nil)
(add-to-list 'package-archives
	     '("emacs-china-melpa" . "http://elpa.emacs-china.org/melpa/"))
(add-to-list 'package-archives
	     '("emacs-china-gnu" . "http://elpa.emacs-china.org/gnu/"))

(defvar fs-package-packages
  '(
    markdown-mode
    smartparens
    ace-window
    flycheck
    company
    company-c-headers
    )
  "Packages for installing."
  )

(if (eq window-system 'ns)
    (add-to-list 'fs-package-packages
		 'exec-path-from-shell)
    )

(defun fs-package-touch-packages ()
  "Check to install package."
  (interactive)
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (message "installing %s" package)
       (package-install package)))
   fs-package-packages))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(fs-package-touch-packages)


(provide 'fs-package)

;;; fs-package.el ends here
