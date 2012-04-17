;;; autoinsert.el --- 
;; $Id: autoinsert-config.el 6 2010-04-30 18:15:53Z michael $
;; Author: Michael S. Joyce <michael@negativespace.net>
;; Keywords: 

(require 'autoinsert)
(auto-insert-mode t)              ;; adds hooks to active mode

(setq load-path
      (cons (expand-file-name ".") load-path))

(if (string= (expand-file-name ".") "/Users/michael/Documents/dot-emacs/autoinsert-config.el")
    (progn (setq auto-insert-directory "/Users/michael/Documents/dot-emacs/emacs-templates"))
  (progn (setq auto-insert-directory "~/.emacs-templates")))

;;; turn off autoinsert prompt
(setq auto-insert-query nil) 

(define-auto-insert "\\.pl" "perl-template.pl")
(define-auto-insert "\\.pm" "perl-template.pm")
(define-auto-insert "\\.t"  "perl-template.t")

(define-auto-insert "\\.tex"  "latex-template.tex")
(define-auto-insert "\\.bib"  "latex-template.bib")
(define-auto-insert "\\.xslt" "xsl-transform.xslt")
(define-auto-insert "\\.xml" "xml-template.xml")
(define-auto-insert "\\.html" "xhtml-template.html")

(provide 'autoinsert-config)
;;; autoinsert-config.el ends here
