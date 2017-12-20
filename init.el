
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
  
  )

;;;; optimization

;; GC threshold
(defconst fs-init-gc-cons-threshold-default
  gc-cons-threshold
  "default GC threashold value")

(defun fs-init-gc-cons-threshold-tweaks (ismax)
  "see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/"
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
