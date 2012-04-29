;;; Dot-emacs.el ---

(global-set-key "\C-c\C-u" 'eval-current-buffer)
(setq load-path  (cons "/usr/share/emacs/site-lisp" load-path))
(setq load-path  (cons (expand-file-name ".") load-path))
(setq load-path  (cons (expand-file-name "/Users/michael/Documents/dotemacs/emacs-lisp") load-path))
(setq load-path  (cons (expand-file-name "/Users/michael/Documents/dotemacs/emacs-local") load-path))
(setq load-path  (cons (expand-file-name "/Users/michael/Documents/dotemacs/emacs-templates") load-path))

(load "auto-modes")
(load "config")
(load "custom-functions")
(load "global-keys")
;; mode-hooks AFTER global-keys: don't blow away the local-set-keys
(load "mode-hooks")
(load "autoinsert-config")
(load "msj-desktop")

; -------------------------------------
; locally installed emacs modes.
(load "ascii-table")
; (load "css-mode")
; (load "drupal-mode")
;(load "find-recursive")
(load "lorem-ipsum")
;(load "lua-mode")
;(load "maxframe")
(load "nxml/rng-auto")
(load "nxhtml/nxhtml-autoload")
;(load "pabbrev")
;(load "rnc-mode")
;(load "scss-mode")
;(load "sql-indent")
(load "tt-mode")

;;; dot-emacs.el ends here
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
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
