;;; mode-hooks.el --- 
;; $Id: mode-hooks.el 6 2010-04-30 18:15:53Z michael $
;; Author: Michael S. Joyce <michael@negativespace.net>
;; Keywords: 

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'xml-mode-hook 'turn-off-auto-fill)
(add-hook 'sgml-mode-hook 'turn-off-auto-fill)
(add-hook 'html-mode-hook 'turn-off-auto-fill)

(add-hook 'sgml-mode-hook '(lambda () (local-set-key [C-return] 'sgml-close-tag)))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(eval-after-load "sql"
      (load-library "sql-indent"))

;;; mode-hooks.el ends here
(provide 'mode-hooks.el)
