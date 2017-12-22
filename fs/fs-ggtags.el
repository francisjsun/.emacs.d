;;; fs-ggtags.el --- ggtags setup

;;; Commentary:
;; 


;;; Code:
(require 'ggtags)

(ggtags-mode)

(define-key ggtags-mode-map (kbd "C-c g d") 'ggtags-find-definition)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-search-history)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)

(provide 'fs-ggtags)

;;; fs-ggtags.el ends here
