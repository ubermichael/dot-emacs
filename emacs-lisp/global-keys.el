;;; global-keys.el --- 
;; $Id: global-keys.el 6 2010-04-30 18:15:53Z michael $
;; Author: Michael S. Joyce <michael@negativespace.net>0123456789!23456789@123456789
;; Keywords: 


;; mac specific stuff.
;; see http://www.webweavertech.com/ovidiu/emacs.html
;; (setq mac-command-key-is-meta      t)
(define-key function-key-map [return] [13])
(global-set-key [(alt a)] 'mark-whole-buffer)
(global-set-key [(alt v)] 'yank)
(global-set-key [(alt c)] 'kill-ring-save)
(global-set-key [(alt x)] 'kill-region)
(global-set-key [(alt s)] 'save-buffer)
(global-set-key [(alt l)] 'goto-line)
(global-set-key [(alt o)] 'find-file)
(global-set-key [(alt f)] 'isearch-forward)
(global-set-key [(alt g)] 'isearch-repeat-forward)
(global-set-key [(alt r)] 'replace-regexp)
;; end of mac specific stuff.

;; make a mouse click do something useful.
(global-set-key "\M-r" 'yank)

;; turn auto indenting on
(global-set-key "\r" 'newline-and-indent)

;; good key bindings.
(global-set-key "\C-cc" 'comment-dwim)

(global-set-key [f1] 'ibuffer)

;; next-error interacts with my-grep() in cool ways.
;; see custom functions.
(global-set-key [f3] 'next-error)
(global-set-key [f7] 'indent-region)
(global-set-key [f8] 'compile)
(global-set-key [f11] 'goto-line)
(global-set-key [S-f11] 'goto-char)
(global-set-key [f12] 'other-window)

(global-set-key "\C-z" 'undo)          
(global-set-key [f4] 'align-regexp)

(global-set-key	[C-tab] 'tab-to-tab-stop) ; shift-tab
(global-set-key [S-tab] 'dabbrev-expand)
(define-key minibuffer-local-map [S-tab] 'dabbrev-expand)

;; see custom-functions.el for these
(global-set-key "\C-cl" 'insert-line)
(global-set-key "\C-c\C-d" 'insert-fancy-date)
(global-set-key "\C-cd" ' insert-date)

(global-set-key "\C-c." 'insert-date)
(global-set-key [M-f12] 'max-frame)

;;
(global-set-key [M-s-left] 'previous-buffer)
(global-set-key [M-s-right] 'next-buffer)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;;
;; (global-set-key [(meta right)] 'forward-word)
;; (global-set-key [(meta left)] 'backward-word)
(global-set-key [(control home)] 'beginning-of-buffer)
(global-set-key [(control end)] 'end-of-buffer)

;;; global-keys.el ends here
(provide 'global-keys)
