(provide 'fs-package)

(require 'package)

;; using emacs-china mirror
(setq package-archives nil)
(add-to-list 'package-archives
	     '("emacs-china-melpa" . "http://elpa.emacs-china.org/melpa/"))
(add-to-list 'package-archives
	     '("emacs-china-gnu" . "http://elpa.emacs-china.org/gnu/"))

(defconst fs-package-packages
  '(
    company
    markdown-mode
    smartparens
    )
  "packages for installing"
  )
(defun fs-package-touch-packages ()
  "check to install package"
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

