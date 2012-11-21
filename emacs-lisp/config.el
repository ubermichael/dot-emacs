;;; config.el --- 
;; $Id: config.el 6 2010-04-30 18:15:53Z michael $
;; Author: Michael S. Joyce <michael@negativespace.net>
;; Keywords: 

(setq user-full-name "Michael S. Joyce")
(setq user-mail-address "michael@negativespace.net")

;; UNICODE.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; highlighting
(show-paren-mode t)
(transient-mark-mode t)
(blink-cursor-mode -1)

;; automatically edit compressed files.
(auto-compression-mode t)

;; don't open new frames -- doesn't work.
(setq one-buffer-one-frame-mode t)

;; Don't add new lines to the end of a file when using down-arrow key 
(setq next-line-add-newlines nil) 
(setq require-final-newline t)

;; Dont show the GNU splash screen 
(setq inhibit-startup-message t) 

;; Make all "yes or no" prompts show "y or n" instead
;; 'y-or-n-p is defined below.
(fset 'yes-or-no-p 'y-or-n-p)

;; make buffer names better
;; configuration comes later, with custom-set-variables...
(load "uniquify")

;; Open unidentified files in text mode 
(setq default-major-mode 'text-mode) 

;; modeline
(display-time)
(line-number-mode 1)
(setq column-number-mode t) 
(which-func-mode t) 

;; turn off the stupid menu bar thingy.
(setq menu-bar-mode nil)

;; allow f-b to expand to foo-bar
(require 'complete)
(partial-completion-mode 1)

;; Do only one line scrolling.
(setq scroll-step 1)

;; things to ingnore, cause emacs doesn't edit them
(setq completion-ignored-extensions 
	  '("~" ".aux" ".a" ".bbl" ".blg" ".dvi" ".elc" ".svn"
      ".hc" ".hi" ".log" ".mlc" ".o" ".toc" ".pdf" ".class"))

(setq make-backup-files nil) ; Don't want any backup files 
(setq auto-save-list-file-name nil) ; Don't want any .saves files 
(setq auto-save-default nil) ; Don't want any auto saving 

;; of course I want to view a read only file.
(setq view-read-only t)

;; iswitchhb - change buffer switching
(iswitchb-mode 1)

;; --------------------------------------------------

;; enable javascript mode
(require 'generic-x)
(add-to-list 'generic-extras-enable-list 'javascript-generic-mode)

;; Give me colours in major editing modes!!!!!
(require 'font-lock)
(global-font-lock-mode t) 

;; make sure that spaces are used instead of tabs which bork xslt
(custom-set-variables '(indent-tabs-mode nil))

;; Set the variable default-tab-width.
(setq tab-width 2)

;; Smarter split-window
(defadvice split-window-vertically
  (after my-window-splitting-advice first () activate)
  (set-window-buffer (next-window) (other-buffer)))

(defadvice split-window-horizontally
  (after my-window-splitting-advice first () activate)
  (set-window-buffer (next-window) (other-buffer)))

(defun yes-or-no-p (arg) 
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'." 
  (y-or-n-p arg))

; -------------------------------------

(require 'ibuf-ext)
(setq ibuffer-expert t)
(setq ibuffer-default-sorting-mode 'filename/process)

(add-hook 'ibuffer-mode-hook 
	  '(lambda ()
	     (ibuffer-auto-mode 1)))
(add-to-list 'ibuffer-never-show-predicates "^\\*")

; ------------------------------------- 
; Make cperl mode suck less.
; http://www.emacswiki.org/emacs/IndentingPerl
; http://www.lemoda.net/emacs/customize-cperl/index.html
(setq cperl-invalid-face nil)
(setq cperl-invalid-face (quote off))
(custom-set-variables '(cperl-indent-parens-as-block t))

; ------------------------------------- 

;;; config.el ends here
(provide 'config)
