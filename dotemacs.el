;;; Michael's .emacs

;;; Dot-emacs.el ---

(global-set-key "\C-c\C-u" 'eval-buffer)
(setq load-path  (cons "/usr/share/emacs/site-lisp" load-path))
(setq load-path  (cons "/usr/local/share/emacs/site-lisp" load-path))
;;; (setq load-path  (cons (expand-file-name ".") load-path))

(if (string= (expand-file-name ".") "/Users/michael/Documents/dot-emacs")
    (progn
      (setq load-path  (cons (expand-file-name "~/Documents/dot-emacs/emacs-lisp") load-path)))
    (progn
      (setq load-path  (cons (expand-file-name "~/.emacs-lisp") load-path))))
  
(load "config")
(load "custom-functions")
(load "global-keys")

; ------------------------------------- 

(require 'package)
; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t) 
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
  '(php-mode
    scss-mode
    less-css-mode
    js2-mode
    jdee
    ejson-mode
    yaml-mode
    )
  )

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
 '(default ((t (
                :stipple nil
                         :background "DarkSlateGrey"
                         :foreground "Wheat"
                         :inverse-video nil
                         :box nil
                         :strike-through nil
                         :overline nil
                         :underline nil
                         :slant normal
                         :weight normal
                         :height 130
                         :width normal
                         :family "apple-andale mono"))))
 '(cperl-array-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(cperl-hash-face ((((class color) (background dark)) (:foreground "Pink"))))
 '(cursor ((t (:background "Wheat")))))
