;;; fs-cc-mode.el --- c/c++ setup

;;; Commentary:
;; 

;;; Code:

(require 'cc-mode)

;; should added before cc-mode enabled
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; clang system include path
(defvar fs-cc-mode-clang-sys-include-path)
;; clang -E -x c++ - -v < /dev/null, ref: https://stackoverflow.com/a/11946295
(let ((clang-output) (sys-include-path))
  (setq clang-output (shell-command-to-string "clang -E -x c++ - -v < /dev/null"))
  (string-match "#include <\\.\\.\\.> search starts here:
\\(\\( .+\n\\)+\\)End of search list." clang-output)
  (setq sys-include-path (match-string 1 clang-output))
  (setq fs-cc-mode-clang-sys-include-path (split-string sys-include-path "\n"))
  )

(defvar fs-cc-mode-additional-sys-include-path
  '(
    ;; "/usr/local"
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
  (let* ((include-path-flags (copy-tree fs-cc-mode-additional-sys-include-path))
	 (idx 0)
	 (lstLen (safe-length include-path-flags)))
    (while (< idx lstLen)
      (let ((path (nth idx include-path-flags)))
	(setcar (nthcdr idx include-path-flags)
		(concat "-I" path)))
      (setq idx (1+ idx)))
    (setq fs-cc-mode-compiler-flags (append fs-cc-mode-additional-cxx-flags include-path-flags))
    (setq company-clang-arguments fs-cc-mode-compiler-flags)
    (setq flycheck-clang-args fs-cc-mode-compiler-flags))
  (message "fs-cc-mode-refresh-compiler-flags, @fs-cc-mode-compiler-flags: %s" fs-cc-mode-compiler-flags)
  )

(require 'company-c-headers)
(defun fs-cc-mode-refresh-company-c-headers-path ()
  "Set `company-c-headers' path."
  (setq company-c-headers-path-system
	(append fs-cc-mode-clang-sys-include-path fs-cc-mode-additional-sys-include-path))
  (message "fs-cc-mode-refresh-company-c-headers-path, @fs-cc-mode-additional-sys-include-path: %s"
           fs-cc-mode-additional-sys-include-path))

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

;; coding style
;; (c-set-style "fs")
(defconst fs-cc-mode--fs-c-style
  '("linux"
    (c-basic-offset . 4)
    (c-offsets-alist
     (inline-open . 0)
     (substatement-open . 0)
     (statement-cont . 0)))
  "FS-C-STYLE.")
(c-add-style "fs" fs-cc-mode--fs-c-style)
(setq c-default-style "fs")

(defun fs-cc-mode-init ()
  "Init."
  (when (fs-cc-mode-is-my-mode)

    
    ;; ff-find-other-file
    (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
    
    ;; company setup
    (add-to-list 'company-backends 'company-c-headers)))


(add-hook 'c-initialization-hook 'fs-cc-mode-init)

(defun fs-cc-mode--flycheck-setup ()
  "Flycheck setup."
  (flycheck-mode)
  (flycheck-select-checker 'c/c++-clang))

;; flycheck setup
(add-hook 'c-mode--common-hook 'fs-cc-mode--flycheck-setup)

;; fs_proj.xml setup
(add-hook 'c-initialization-hook 'fs-cc-mode-auto-setup-proj-file)

;; interactive utils
(defun fs-cc-mode-add-sys-include-path (sys-path)
  "Adding system include path.
Argument SYS-PATH new system path."
  (interactive "Dnew system include path:")
  (message "hello add sys")
  (add-to-list 'fs-cc-mode-additional-sys-include-path sys-path)
  (fs-cc-mode-refresh))

;; fs c++ proj file(fs_proj.xml)
;; <inc_dir>
;; <sys>
;; "h1;h2;..."
;; </sys>
;; <user>
;; "h1;h2;..."
;; </user>
;; </inc_dir>
(defconst fs-cc-mode--fs-proj-file-name "fs_proj.xml")
(defvar fs-cc-mode-current-proj-file nil
  "Current proj file.")

(defun fs-cc-mode-set-proj-file (fs-proj-file)
  "Set current proj file.
Argument FS-PROJ-FILE fs_proj.xml path"
  (interactive "ffs_proj.xml path: ")
  (setq fs-cc-mode-current-proj-file fs-proj-file)
  (fs-cc-mode--parse-proj-file fs-cc-mode-current-proj-file))

(defun fs-cc-mode-auto-setup-proj-file ()
  "Find fs_proj.xml and parse automatically."
  (interactive)
  (let ((found nil)
        (start-idx 0)
        (dirs)
        (dir-idx 0)
        (len-dirs))

    ;; find all dirs
    (while (setq start-idx (string-match-p "/" default-directory start-idx))
      (add-to-list 'dirs (substring default-directory 0 start-idx))
      (setq start-idx (+ start-idx 1)))
    (setq len-dirs (length dirs))
    ;; iterate all dirs
    (while (and (not found) (< dir-idx len-dirs))
      (let ((current-dir)
            (file-idx 0)
            (files)
            (files-count)
            (current-file))
        (setq current-dir (nth dir-idx dirs))

        (setq files (directory-files current-dir))
        (setq files-count (length files))
        ;; iterate all files in dir
        (while (and (not found) (< file-idx files-count))
          (setq current-file (nth file-idx files))
          (when (string-equal current-file fs-cc-mode--fs-proj-file-name)
            (setq found current-dir))
          (setq file-idx (+ file-idx 1)))
        (setq dir-idx (+ dir-idx 1))))
    (if found
        (progn
          (message "%s found @dir: %s" fs-cc-mode--fs-proj-file-name found)
          (fs-cc-mode--parse-proj-file
           (expand-file-name fs-cc-mode--fs-proj-file-name found)))
      (message "%s not found" fs-cc-mode--fs-proj-file-name))))

(defvar fs-cc-mode-fs-proj-src-files
  nil "Fs proj src files.")

(require 'xml)
(defun fs-cc-mode--parse-proj-file (proj-file)
  "Parse a proj file.
Argument PROJ-FILE fs_proj file path."
  (let* (
         (xml-root (xml-parse-file proj-file nil nil))
         (xml-root (car xml-root))      ; get rid of first parentheses:((node))
         (inc-dir-node (car (xml-get-children xml-root 'inc_dir)))
         (sys-inc-dir (xml-get-attribute inc-dir-node 'sys))
         (user-inc-dir (xml-get-attribute inc-dir-node 'user))
         (src-node (car (xml-get-children xml-root 'src)))
         (src-files (xml-get-attribute src-node 'file))
         ;; (sys-inc-dir (car (xml-get-children xml-root 'sys)))
         ;; (xml-get-attribute)
         ;; (user-inc-dir (car (xml-get-children xml-root 'user)))
         ;; (sys-inc-dir (nth 2 sys-inc-dir))
         ;; (user-inc-dir (nth 2 user-inc-dir))
         (inc-dir)
         )
    (setq fs-cc-mode-fs-proj-src-files (split-string src-files ";" t))
    (let ((post-edit-inc-dir (lambda (inc-dir)
                               (let ((inc-dir-value))
                                 ;; rm character '\n' '"'
                                 (setq inc-dir-value (eval inc-dir))
                                 (setq inc-dir-value (seq-remove (lambda (elt) ;will break into list
                                                                   (or (= elt ?\") (= elt ?\n)))
                                                                 inc-dir-value))
                                 (setq inc-dir-value (concat inc-dir-value)) ;concat into a string again
                                 ;; split into list
                                 (set inc-dir (split-string inc-dir-value ";" t))))))
      
      (funcall post-edit-inc-dir 'sys-inc-dir)
      (funcall post-edit-inc-dir 'user-inc-dir))

    (setq inc-dir (append sys-inc-dir user-inc-dir))

    ;; (setq inc-dir (concat sys-inc-dir user-inc-dir))
    (message "inc-dir: %s, len: %s" inc-dir (length inc-dir))
    (when inc-dir
      (let (
            (len-inc-dir (safe-length inc-dir))
            (idx 0)
            (cur-dir))
        (while (< idx len-inc-dir)
          (setq cur-dir (nth idx inc-dir))
          (when (not (member cur-dir fs-cc-mode-additional-sys-include-path))
            (add-to-list 'fs-cc-mode-additional-sys-include-path cur-dir))
          (setq idx (+ idx 1))
          )
        (fs-cc-mode-refresh)))
    ))

(defconst fs-cc-mode-license-header-template
  "/*
 * Copyright (c) %s, F.S.. All rights reserved.
 */
")

(defconst fs-cc-mode-license-header
  (format fs-cc-mode-license-header-template
          (format-time-string "%Y" (current-time))))

;; declare copyright
(defun fs-cc-mode-fs-proj-declare-copyright (&optional COPYRIGHT)
  "Insert COPYRIGHT statement to all fs-proj src files."
  (interactive "sCopyright: ")
  (dolist (src-file fs-cc-mode-fs-proj-src-files)
    (fs-cc-mode-declare-copyright src-file COPYRIGHT)))

(defun fs-cc-mode-declare-copyright (FILE-PATH &optional COPYRIGHT)
  "Insert COPYRIGHT statement at the beginning of FILE-PATH."
  (interactive "fFile path: \nsCopyright: ")
  (if (equal "" COPYRIGHT)
      (setq COPYRIGHT fs-cc-mode-license-header)
    (setq COPYRIGHT (concat "/*\n" " * " COPYRIGHT "\n */\n\n"))) ;remove ending '\n'
  (let ((origin-file-content)
        (old-copyright-begin-idx)
        (old-copyright-end-idx)
        (old-copyright-idx))
    (setq origin-file-content
          (with-temp-buffer (insert-file-contents FILE-PATH)
                            (buffer-string)))
    (setq old-copyright-begin-idx (string-match-p "/\*" origin-file-content))
    (when old-copyright-begin-idx
      (setq old-copyright-end-idx
            (string-match-p "\*/" origin-file-content old-copyright-begin-idx))
      (when old-copyright-end-idx
        (setq old-copyright-idx
              (string-match-p "Copyright" origin-file-content old-copyright-begin-idx))
        (when (and old-copyright-idx (< old-copyright-idx old-copyright-end-idx))
          ;; old copyright statement found.
          (setq origin-file-content
                ;; cut old copyright out
                (substring origin-file-content (+ old-copyright-end-idx 2)))
          ;; remove all heading '\n'
          (let* ((ele-idx 0)
                 (ele (elt origin-file-content ele-idx))
                 (a-newline-p (equal ?\n ele)))
            (while a-newline-p
              (setq ele-idx (+ 1 ele-idx))
              (setq ele (elt origin-file-content ele-idx))
              (setq a-newline-p (= ?\n ele)))
            (setq origin-file-content (substring origin-file-content ele-idx))))))
    
    (setq origin-file-content (concat COPYRIGHT origin-file-content))
    (write-region origin-file-content nil FILE-PATH)))

;; class template
(defconst fs-cc-mode-header-template
  (concat fs-cc-mode-license-header
          "#pragma once

class %s
{

};"))

(defconst fs-cc-mode-cpp-template
  (concat fs-cc-mode-license-header
          "#include \"%s.h\""))

(defun fs-cc-mode-create-class (CLASS-NAME PATH)
  "Create a .h and .cpp files cooresponding to CLASS-NAME in PATH."
  (interactive "sclass-name: \nDpath:")
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
  (interactive "header-name: \nDpath:")
  (let ((header-template fs-cc-mode-single-header-template)
        (file-name (concat PATH "/" HEADER-NAME ".h")))
    (write-region header-template nil file-name nil nil nil t)
    (find-file file-name)
    ))

(provide 'fs-cc-mode)

(require 'fs-gdb)
;;; fs-cc-mode.el ends here
