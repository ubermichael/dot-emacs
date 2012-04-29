;;; desktop.el --- 

;; Saving Emacs Sessions - Useful when you have a bunch of source
;; files open and you don't want to go and manually open each one,
;; especially when they are in various directories. Page 377 of the
;; GNU Emacs Manual says: "The first time you save the state of the
;; Emacs session, you must do it manually, with the command M-x
;; desktop-save. Once you have dome that, exiting Emacs will save the
;; state again -- not only the present Emacs session, but also
;; subsequent sessions. You can also save the state at any time,
;; without exiting Emacs, by typing M-x desktop-save again.

(load "desktop")
(desktop-save-mode 1)
(setq desktop-path (list "."))
(setq desktop-save 'if-exists)

;; not all buffers should be reopened.
(setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	        "\\)$"))
   (add-to-list 'desktop-modes-not-to-save 'dired-mode)
   (add-to-list 'desktop-modes-not-to-save 'Info-mode)
   (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


(provide 'msj-desktop)
;;; desktop.el ends here
