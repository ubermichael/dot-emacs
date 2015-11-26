;;; auto-modes.el --- 
;; $Id: auto-modes.el 6 2010-04-30 18:15:53Z michael $
;; Author: Michael S. Joyce <michael@negativespace.net>
;; Keywords: 

(setq auto-mode-alist
      (append
       (list         
        (cons "\\.css$" 'css-mode)
        (cons "\\.log$" 'compilation-mode)        
        (cons "\\.mxi$" 'nxml-mode)
        (cons "\\.php$" 'php-mode)
        (cons "\\.rnc$" 'rnc-mode)
        (cons "\\.tt$" 'tt-mode)
        (cons "\\.xml$" 'nxml-mode)        
        (cons "\\.xmp$" 'nxml-mode)
        (cons "\\.xsd$" 'nxml-mode)
        (cons "\\.xsl$" 'nxml-mode)
        (cons "\\.xslt$" 'nxml-mode)
        (cons "\\.xmap$" 'nxml-mode)
	(cons "\\.t$"    'cperl-mode)
        (cons "\\.rtf$"  'rtf-mode)
        (cons "\\.md$"   'markdown-mode)
        )
       auto-mode-alist))
(defalias 'perl-mode 'cperl-mode)

;;; auto-modes.el ends here
(provide 'auto-modes)
