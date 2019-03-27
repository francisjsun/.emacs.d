;;; fs-package.el --- install package iff not installed

;;; Commentary:
;; 


;;; Code:

(require 'package)

;; using melpa
(setq package-archives nil)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
 	     '("melpa-stable" . "http://stable.melpa.org/packages/"))

(add-to-list 'package-archive-priorities '((melpa-stable . 1)))


(defvar fs-package-packages
  '(
    markdown-mode
    smartparens
    ace-window
    rainbow-delimiters
    flycheck
    company
    company-c-headers
    ;; ggtags
    ;; rtags
    ;; flycheck-rtags
    cmake-mode
    lua-mode
    glsl-mode
    irony
    company-irony
    flycheck-irony
    elpy
    ;; company-glsl
    )
  "Packages for installing."
  )

(when (eq window-system 'ns)
    (add-to-list 'fs-package-packages
		 'exec-path-from-shell))


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
