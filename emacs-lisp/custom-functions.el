;;; custom-functions.el --- 
;; $Id: custom-functions.el 6 2010-04-30 18:15:53Z michael $
;; Author: Michael S. Joyce <michael@negativespace.net>
;; Keywords: 

; ====================================
; window management fun
(defun max-frame ()
  (interactive)
  "make a new buffer with maximum (for my laptop) size"
  (select-frame (make-frame '((width . 176) (height . 55)
                             (top . 0) (left . 0)))))

;; --------------------------------------------------
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y %k:%M:%S %z\n")))

(defun insert-line ()
  "insert a line of ----"
  (interactive)
  (if comment-start (insert comment-start))
  (insert " ------------------------------------- ")
  (if comment-end (insert comment-end))
  (insert "\n"))

(defun insert-fancy-date()
  "insert a fancily formatted date"
  (interactive)
  (insert-line)
  (if comment-start (insert comment-start))
  (insert " ")
  (insert-date)
  (if comment-end (insert comment-end))
  (insert-line))

;; Convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun open-dot-emacs () 
  "opening-dot-emacs" 
  (interactive)				   ;this makes the function a command too 
  (find-file "~/.emacs" ))

(defun eval-dot-emacs () 
  "re-evaluating-dot-emacs"
  (interactive)
  (find-file "~/.emacs")
  (eval-buffer ".emacs")
  (kill-buffer ".emacs"))

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
	  (beginning-of-line)
	(back-to-indentation)))

(defun s-switch-to-buffer ()
  "A shell around switch-to-buffer which removes the current buffer from the completion list."
  (interactive)
  (setq s-remove-first-completion 't)
  (switch-to-buffer (read-buffer "Switch to buffer: " (other-buffer))))

;; ASCII table, in a new buffer.
;; TODO: make it a real table.
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)  (switch-to-buffer "*ASCII*")  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))  (let ((i 0))
    (while (< i 254)      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))  (beginning-of-buffer))


;; --------------------------------------------------
;; http://www.emacswiki.org/cgi-bin/wiki/RenumberList
;; renumber paragraph, starting from point

(defun renumber (&optional num)
      "Renumber the list items in the current paragraph,
    starting at point."
      (interactive "p")
      (setq num (or num 1))
      (let ((end (save-excursion
                   (forward-paragraph)
                   (point))))
        (while (re-search-forward "^[0-9]+" end t)
          (replace-match (number-to-string num))
          (setq num (1+ num)))))

;; renumber selection.

(defun renumber-list (start end &optional num)
      "Renumber the list items in the current START..END region.
    If optional prefix arg NUM is given, start numbering from that number
    instead of 1."
      (interactive "*r\np")
      (save-excursion
        (goto-char start)
        (setq num (or num 1))
        (save-match-data
          (while (re-search-forward "^[0-9]+" end t)
            (replace-match (number-to-string num))
            (setq num (1+ num))))))

;; --------------------------------------------------


;;; custom-functions.el ends here
(provide 'custom-functions)
