;;; Dot-emacs.el ---
;; $Id: dot-emacs.el 7 2010-04-30 18:18:56Z michael $
;; Author: Michael S. Joyce <michael@negativespace.net>
;; Keywords: 

(global-set-key "\C-c\C-u" 'eval-current-buffer)
;; (require 'mac-key-mode)
;; (mac-key-mode 1)

(setq load-path  (cons "/usr/share/emacs/site-lisp" load-path))

(if (string= (expand-file-name ".") "/Users/michael/Documents/dot-emacs")
    (progn (setq load-path  (cons (expand-file-name ".") load-path))
           (setq load-path  (cons (expand-file-name "/Users/michael/Documents/dot-emacs/emacs-lisp") load-path))
           (setq load-path  (cons (expand-file-name "/Users/michael/Documents/dot-emacs/emacs-local") load-path)))
  (progn (setq load-path (cons (expand-file-name "~/.emacs-lisp") load-path))
         (setq load-path (cons (expand-file-name "~/.emacs-local") load-path))))

(load "auto-modes")(defalias 'perl-mode 'cperl-mode)
(load "config")
(load "custom-functions")
(load "global-keys")
;; mode-hooks AFTER global-keys: don't blow away the local-set-keys
(load "mode-hooks")
(load "autoinsert-config")
(load "rnc-mode")

;; add nxml mode automagically
(load "nxml/rng-auto")
(load "nxhtml/nxhtml-autoload")
(load "tt-mode")

;; xquery mode is goodness.
(load "xquery-mode")

(load "ascii-table")

(load "drupal-mode")

;;; dot-emacs.el ends here
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "DarkSlateGrey" :foreground "Wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :family "apple-andale mono"))))
 '(cperl-array-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(cperl-hash-face ((((class color) (background dark)) (:foreground "Pink"))))
 '(cursor ((t (:background "Wheat")))))
