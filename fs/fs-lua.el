;;; fs-lua.el --- lua-mode setup

;(require 'lua-mode)

;;; Commentary:
;; 

;;; Code:

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(provide 'fs-lua)

;;; fs-lua.el ends here
