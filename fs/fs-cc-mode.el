;;; fs-cc-mode.el --- c/c++ setup

;;; Commentary:
;; 

;;; Code:

(require 'cc-mode)

;; should added before cc-mode enabled
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; gcc system include path
(defvar fs-cc-mode-gcc-sys-include-path)
;; g++ -E -x c++ - -v
(let ((g++-output) (sys-include-path))
  (setq g++-output (shell-command-to-string "g++ -E -x c++ - -v"))
  (string-match "#include <\\.\\.\\.> search starts here:
\\(\\( .+\n\\)+\\)End of search list." g++-output)
  (setq sys-include-path (match-string 1 g++-output))
  (setq fs-cc-mode-gcc-sys-include-path (split-string sys-include-path "\n"))
  )

(defvar fs-cc-mode-addtional-sys-include-path
  '(
    "/usr/local"
    )
  "Addtional system include path.")

(defvar fs-cc-mode-additional-cxx-flags
  '(
    "-std=c++11"
    "-Wno-pragma-once-outside-header"	;remove warning: pragma once in main file
    )
  "Additional cxx flags.")

(defvar fs-cc-mode-compiler-flags
  nil
  "Final compiler flags.")

(require 'company-clang)
(require 'flycheck)
(defun fs-cc-mode-refresh-compiler-flags ()
  "Evaluate compiler flags from include path& cxx flags.
And then set into company-clang-arguments and flycheck-clang-args"
  (let* ((include-path-flags (copy-tree fs-cc-mode-addtional-sys-include-path))
	(idx 0)
	(lstLen (safe-length include-path-flags)))
    (while (< idx lstLen)
      (let ((path (nth idx include-path-flags)))
	(setcar (nthcdr idx include-path-flags)
		(concat "-I" path)))
      (setq idx (1+ idx)))
    (setq fs-cc-mode-compiler-flags (append fs-cc-mode-additional-cxx-flags include-path-flags))
    (setq company-clang-arguments fs-cc-mode-compiler-flags)
    (setq flycheck-clang-args fs-cc-mode-compiler-flags)))

(require 'company-c-headers)
(defun fs-cc-mode-refresh-company-c-headers-path ()
  "Set `company-c-headers' path."
  (setq company-c-headers-path-system
	(append fs-cc-mode-gcc-sys-include-path fs-cc-mode-addtional-sys-include-path)))

;; refresh
(defun fs-cc-mode-refresh ()
  "Refresh compiler flags and company-c-headers-path-*."
  (fs-cc-mode-refresh-compiler-flags)
  (fs-cc-mode-refresh-company-c-headers-path))

;; init
(require 'company-clang)
(require 'company-c-headers)

(defconst fs-cc-mode-name
  '("c-mode"
    "c++-mode"
    ))

(defun fs-cc-mode-is-my-mode ()
  "Return t if 'major-mode' is on of my 'mode-name' else return nil."
  (catch 'ret
    (dolist (m fs-cc-mode-name)
      (when (string-equal major-mode m)
	(throw 'ret t)))
    (throw 'ret nil)))

(defun fs-cc-mode-init ()
  "Init."
  (when (fs-cc-mode-is-my-mode)
    ;; coding style
    (c-set-style "linux")
    (setq c-basic-offset 4)
    
    ;; ff-find-other-file
    (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
    
    ;; company setup
    (add-to-list 'company-backends 'company-c-headers)

    ;; flycheck setup
    (flycheck-mode)
    (flycheck-select-checker 'c/c++-clang)

    ;; gtags
    ;; (require 'fs-ggtags)
    
    ;; first refresh
    (fs-cc-mode-refresh)))

(add-hook 'c-mode-common-hook 'fs-cc-mode-init)

;; interactive utils
(defun fs-cc-mode-add-sys-include-path (sys-path)
  "Adding system include path.
Argument SYS-PATH new system path."
  (interactive "Dnew system include path:")
  (add-to-list 'fs-cc-mode-addtional-sys-include-path sys-path)
  (fs-cc-mode-refresh))

(defconst fs-cc-mode-license-header-template
  "/*
* Copyright (c) %s, F.S.. All rights reserved.
*/

")

(defconst fs-cc-mode-license-header
  (format fs-cc-mode-license-header-template
          (format-time-string "%Y" (current-time))))

;; class template
(defconst fs-cc-mode-header-template
  (concat fs-cc-mode-license-header
          "#pragma once

class %s
{

};"))

(defconst fs-cc-mode-cpp-template
"#include \"%s.h\"")

(defun fs-cc-mode-create-class (CLASS-NAME PATH)
  "Create a .h and .cpp files cooresponding to CLASS-NAME in PATH."
  (interactive "sclass-name:\nDpath:")
  (let ((header-template (format fs-cc-mode-header-template CLASS-NAME))
	(cpp-template (format fs-cc-mode-cpp-template CLASS-NAME))
	(file-name (concat PATH "/" CLASS-NAME ".")))
    (write-region cpp-template nil (concat file-name "cpp") nil nil nil t)
    (write-region header-template nil (concat file-name "h") nil nil nil t)
    (find-file (concat file-name "h"))))


;; single header template
(defconst fs-cc-mode-single-header-template
  (concat fs-cc-mode-license-header
          "#pragma once"))

(defun fs-cc-mode-create-single-header (HEADER-NAME PATH)
  "Create a .h file cooresponding to HEADER-NAME in PATH."
  (interactive "sheader-name:\nDpath:")
  (let ((header-template fs-cc-mode-single-header-template)
        (file-name (concat PATH "/" HEADER-NAME ".h")))
    (write-region header-template nil file-name nil nil nil t)
    (find-file file-name)
    ))

(provide 'fs-cc-mode)

(require 'fs-gdb)
;;; fs-cc-mode.el ends here
