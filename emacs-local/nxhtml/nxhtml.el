;;; nxhtml.el --- Edit XHTML files

;; Copyright (C) 2005 by Lennart Borgman
;; Parts are from Peter Heslin (see below)

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-08-05
(defconst nxhtml:version "0.70") ;;Version:
;; Last-Updated: Thu Jun 01 18:21:25 2006 (7200 +0200)
;; Keywords: languages
;; - Features that might be required by this library:
;;
;;   `appmenu', `appmenu-fold', `browse-url', `cl', `compile',
;;   `easymenu', `fold-dwim', `fupd', `hexcolor', `hideshow',
;;   `html-chklnk', `html-inlined', `html-move', `html-pagetoc',
;;   `html-site', `html-toc', `html-upl', `html-wtoc', `mail-prsvr',
;;   `mm-util', `nxml-enc', `nxml-glyph', `nxml-mode', `nxml-outln',
;;   `nxml-rap', `nxml-util', `outline', `tidy', `url-expand',
;;   `url-methods', `url-parse', `url-util', `url-vars',
;;   `xhtml-help', `xmlpe', `xmltok'.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  The purpose of nxhtml.el is to add some features that are useful
;;  when editing XHTML files to nxml-mode.  For more information see
;;  `nxhtml-mode'.
;;
;;
;;  Usage:
;;
;;  Put this file in `load-path'. In your .emacs:
;;
;;     ;; Load nxml according to the instructions, ie something like:
;;     (load "your-path/nxml-mode-20041004/rng-auto.el")
;;
;;     ;; Then autoload nxhtml-mode:
;;     (autoload 'nxhtml-mode "nxhtml" "Mode for editing XHTML files - based on nxml-mode." t)
;;
;;     ;; For file associations you can use:
;;     (require 'fmode)
;;     (fmode-replace-default-mode 'html-mode 'nxhtml-mode)
;;     (fmode-replace-default-mode 'xml-mode 'nxml-mode)
;;
;;
;;  Tip: Why not put all these in a .nxml file that you load in your
;;  .emacs?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2006-04-25: Added completion for href, src etc. Removed xhtmlin.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'nxml-mode)
(require 'url-parse)
(require 'url-expand)
(eval-when-compile (require 'cl))

(defvar nxhtml-req-features
  (let ((req-features
         '(
           (html-site    "Web sites you define"
                         "html-site.el" "0.1")
           (html-chklnk  "Checking links in site"
                         "html-chklnk.el" "0.2")
           (html-move    "Moving files in web sites"
                         "html-move.el" "0.31")
           (html-pagetoc "Page TOC"
                         "html-pagetoc.el" "0.84")
           (html-toc     "Web site TOC"
                         "html-toc.el" "0.4")
           (html-wtoc    "Merge pages and Web Site TOC"
                         "html-wtoc.el" "0.2")
           (html-upl     "Upload web sites"
                         "html-upl.el" "0.2")
           (html-inlined "Editing of inlined code"
                         "html-inlined.el" "2.2")
           (xmlpe        "Editing of XHTML fragments"
                         "xmlpe.el" "0.56")
           (tidy         "Run HTML tidy program"
                         "tidy.el")
           (xhtml-help   "HTML+CSS help"
                         "xhtml-help.el" "0.56")
           (hexcolor     "Hex color help functions"
                         "hexcolor.el" "0.5")
           (fold-dwim    "Folding on headers and tags"
                         "fold-dwim.el")
           (appmenu      "Popup menu"
                         "appmenu.el" "0.51")
           (appmenu-fold "Popup menu entries for folding"
                         "appmenu-fold.el" "0.51" appmenu fold-dwim)
           )
         ))
    (dolist (extf req-features)
      (require (car extf) nil t))
    (when (featurep 'html-inlined)
      (html-inlined-add-key-to-modes))
    req-features))


(defun nxhtml-make-library-link(beg end)
  (let ((library (buffer-substring-no-properties beg end)))
    (make-text-button beg end
                      'action (lambda (button)
                                (find-library
                                 (button-get button 'lib-name)))
                      'lib-name library
                      'face 'button)))

(defun nxhtml-feature-insert(ok msg)
  (put-text-property 0 (length msg)
                     'face (list
                            (cons 'foreground-color
                                  (if ok "RGB:00/cc/00"
                                    "RGB:cc/00/00")))
                     msg)
  (insert msg))

(defun nxhtml-feature-check(feat-entry silent)
  (let ((feature     (nth 0 feat-entry))
        (description (nth 1 feat-entry))
        (file        (nth 2 feat-entry))
        (need-ver    (nth 3 feat-entry))
        (need-list   (cddddr feat-entry))
        (ok))
    (if (featurep feature)
        (let* (
               (feat-versym (read (format "%s:version" feature)))
               (feat-ver (condition-case err
                             (symbol-value feat-versym)
                           (error nil)))
               (feat-vok (or (not need-ver)
                             (and feat-ver
                                  (version<= need-ver feat-ver))))
               (need-ok (or (not need-list)
                            (let ((has t))
                              (dolist (n need-list)
                                (unless (featurep n)
                                  (setq has nil)))
                              has))))
          (setq ok (and feat-vok need-ok))
          (unless silent
            (nxhtml-feature-insert
             ok
             (concat (format "%31s -- " description)
                     (if ok
                         (format "supported by %s\n" file)
                       (concat "found " file
                               " but needs"
                               (if feat-vok ""
                                 (format " version %s" need-ver))
                               (if (or feat-vok need-ok) "" " and")
                               (if need-ok ""
                                 (format " also %s" need-list))
                               "\n"))))
            (unless (string= file
                             (file-name-nondirectory (feature-file feature)))
              (insert (make-string (+ 31 4) ?\ ) "** Bad file name: " file "\n"))))
      (unless silent
        (nxhtml-feature-insert
         nil (format "%31s -- support missing, can't find %s\n"
                     description file))))
    ok))

(defun nxhtml-features-check()
  "Check if external modules used by `nxhtml-mode' are found.
See this function for more information."
  (interactive)
  (switch-to-buffer (get-buffer-create "*NXhtml Feature Check*") t)
  (help-mode)
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((s (concat "Elisp modules used by Nxhtml Mode version " nxhtml:version ":")))
      (put-text-property 0 (length s)
                         'face '( :weight bold :height 1.4)
                         s)
      (insert s "\n\n"))
    (dolist (feat-entry nxhtml-req-features)
      (nxhtml-feature-check feat-entry nil))
    (goto-char (point-min))
    (while (search-forward-regexp "[-a-zA-Z]+\\.el" nil t)
      (nxhtml-make-library-link
       (match-beginning 0)
       (match-end 0)))
    (goto-char (point-min)))
  (set-buffer-modified-p nil))

(defun nxhtml-all-features-found()
  (let ((all t))
    (dolist (feat-entry nxhtml-req-features)
      ;;(unless (featurep (car extf))
      (unless (nxhtml-feature-check feat-entry t)
        (setq all nil)))
    all))

;;(defun nxhtml-nxml-fontify-attribute (att &optional namespace-declaration)
;;"Holds the original `nxml-fontify-attribute' function.")
;;(fset 'nxhtml-nxml-fontify-attribute (symbol-function 'nxml-fontify-attribute))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redefine nxml-fontify-attribute

(defun nxml-fontify-attribute (att &optional namespace-declaration)
  (let ((name-face 'nxml-attribute-local-name-face))
    (when (eq major-mode 'nxhtml-mode)
      (let ((att-str (buffer-substring-no-properties
		      (xmltok-attribute-name-start att)
		      (xmltok-attribute-name-end att))))
	(when (or (string= att-str "href")
                  (string= att-str "src"))
	  (setq name-face 'nxhtml-link-face)
            (put-text-property (xmltok-attribute-name-start att)
                               (xmltok-attribute-name-end att)
                               'keymap nxhtml-href-keymap)
            (put-text-property (xmltok-attribute-name-start att)
                               (xmltok-attribute-name-end att)
                               'mouse-face 'highlight))))
    (if namespace-declaration
	(nxml-fontify-qname (xmltok-attribute-name-start att)
			    (xmltok-attribute-name-colon att)
			    (xmltok-attribute-name-end att)
			    'nxml-namespace-attribute-xmlns-face
			    'nxml-namespace-attribute-colon-face
			    'nxml-namespace-attribute-prefix-face
			    'nxml-namespace-attribute-xmlns-face)
      (nxml-fontify-qname (xmltok-attribute-name-start att)
			  (xmltok-attribute-name-colon att)
			  (xmltok-attribute-name-end att)
			  'nxml-attribute-prefix-face
			  'nxml-attribute-colon-face
			  name-face)))
  (let ((start (xmltok-attribute-value-start att))
	(end (xmltok-attribute-value-end att))
	(refs (xmltok-attribute-refs att))
	(delimiter-face (if namespace-declaration
			    'nxml-namespace-attribute-value-delimiter-face
			  'nxml-attribute-value-delimiter-face))
	(value-face (if namespace-declaration
			'nxml-namespace-attribute-value-face
		      'nxml-attribute-value-face)))
    (when start
      (nxml-set-face (1- start) start delimiter-face)
      (nxml-set-face end (1+ end) delimiter-face)
      (while refs
	(let* ((ref (car refs))
	       (ref-type (aref ref 0))
	       (ref-start (aref ref 1))
	       (ref-end (aref ref 2)))
	  (nxml-set-face start ref-start value-face)
	  (nxml-apply-fontify-rule ref-type ref-start ref-end)
	  (setq start ref-end))
	(setq refs (cdr refs)))
      (nxml-set-face start end value-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Folding etc. This part is taken from
;; http://www.emacswiki.org/cgi-bin/wiki/NxmlModeForXHTML and was
;; originally written by Peter Heslin. It requires fold-dwim.el.

(when (featurep 'fold-dwim)

  (defun nxhtml-setup-for-fold-dwim ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
    (make-local-variable 'outline-level)
    ;;(setq outline-level 'my-xhtml-outline-level)
    (setq outline-level 'nxhtml-outline-level)
    (outline-minor-mode 1)
    (hs-minor-mode 1)
    (add-to-list 'hs-special-modes-alist
                 '(nxml-mode
                   "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                   ""
                   "<!--" ;; won't work on its own; uses syntax table
                   (lambda (arg) (nxhtml-forward-element))
                   nil))
    (when (featurep 'appmenu-fold)
      (appmenu-fold-setup))
    )

  (defun nxhtml-outline-level ()
    (save-excursion (re-search-forward html-outline-level))
    (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
      (if (eq (length tag) 2)
          (- (aref tag 1) ?0)
        0)))


  (defun nxhtml-forward-element ()
    (let ((nxml-sexp-element-flag))
      (setq nxml-sexp-element-flag (not (looking-at "<!--")))
      (unless (looking-at outline-regexp)
        (condition-case nil
            (nxml-forward-balanced-item 1)
          (error nil)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defface nxhtml-link-face
  `((((class color) (background light)) (:foreground "blue" :underline t))
    (((class color) (background dark)) (:foreground "cyan" :underline t)))
  "Face used to highlight attributes that are links."
  :group 'nxml-highlighting-faces)


(defvar nxhtml-href-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'nxhtml-href-click)
    (define-key map "\C-m" 'nxhtml-href-click)
    (define-key map [(tab)]       'nxhtml-next-href)
    (define-key map [(shift tab)] 'nxhtml-prev-href)
    map))

(defvar nxhtml-link-regexp "\\(?:href\\|src\\)")

(defun nxhtml-next-href()
  (interactive)
  (let (next
        (last -1)
        (wrapped 0)
        (start (point)))
    (forward-char 1)
    (while (and (> 2 wrapped)
                (not next))
      (setq next (search-forward-regexp nxhtml-link-regexp nil t))
      (unless next
        (goto-char (point-min))
        (setq wrapped (1+ wrapped))
        (setq next (search-forward-regexp nxhtml-link-regexp nil t)))
      (when (and next
                 (not (= next last)))
        (setq last next)
        (backward-char)
        (unless (eq nxhtml-href-keymap (get-text-property (point) 'keymap))
          (setq next nil))
        ))
    (if (and next
             ;;(not (= next last))
             )
        (backward-char (1- (length (match-string 0))))
      (goto-char start)
      (message "No next href"))))

(defun nxhtml-prev-href()
  (interactive)
  (let (next
        (last -1)
        (wrapped 0)
        (start (point)))
    (backward-char 1)
    (while (and (> 2 wrapped)
                (not next))
      (setq next (search-backward-regexp nxhtml-link-regexp nil t))
      (unless next
        (goto-char (point-max))
        (setq wrapped (1+ wrapped))
        (setq next (search-backward-regexp nxhtml-link-regexp nil t)))
      (when (and next
                 (not (= next last)))
        (setq last next)
        (unless (eq nxhtml-href-keymap (get-text-property (point) 'keymap))
          (setq next nil))))
    (if (and next
             ;;(not (= next last))
             )
        (message "")
      (goto-char start)
      (message "No previous href"))))



(defun nxhtml-find-base-href()
  "Return base href found in the current file."
  (let ((base-href))
    (save-excursion
      (goto-char (point-min))
      (while (and (not base-href)
		  (search-forward-regexp "<!--[^!]*-->\\|<base[[:space:]]" nil t))
	(when (equal " " (char-to-string (char-before)))
	  (backward-char 6)
	  (when (looking-at "<base [^>]*href *= *\"\\(.*?\\)\"")
	    (setq base-href (match-string-no-properties 1))))))
    base-href))

(defun nxhtml-get-link-at(point attrname-regexp)
  (save-excursion
    (goto-char point)
    ;;(forward-char (length attrname))
    (forward-char 4)
    (when (search-backward-regexp attrname-regexp nil t)
      (when (looking-at (concat attrname-regexp " *= *\"\\(.*?\\)\""))
        (when (and (<= (match-beginning 0) point)
                   (< point (match-end 0)))
          (match-string-no-properties 1))))))

(defun nxhtml-possible-href-actions(link)
  (let ((urlobj (url-generic-parse-url link))
	(edit nil)
	(possible nil))
    (cond ((member (url-type urlobj) '("http" "https"))
	   (add-to-list 'possible (cons 'view-web link)))
	  ((member (url-type urlobj) '("mailto"))
	   (add-to-list 'possible (cons 'mailto link)))
	  ((url-host urlobj)
	   (message "Do not know how to handle this URL"))
	  (t (setq edit t)))
    (when edit
      (let ((base-href (nxhtml-find-base-href)))
	(when base-href
	  (let ((baseobj (url-generic-parse-url base-href)))
	    (setq edit nil)
	    (cond ((member (url-type baseobj) '("http" "https"))
		   (add-to-list 'possible (cons 'view-web-base (url-expand-file-name href-val base-href))))
		  ((url-host urlobj)
		   (message "Do not know how to handle this URL"))
		  (t (setq edit t)))))
	(when edit
	  (let* ((full (split-string (url-filename urlobj) "#"))
		 (file (nth 0 full))
		 (anchor (nth 1 full))
		 )
	    (when (equal file "")
	      (setq file (buffer-file-name)))
	    (when base-href
              ;; We not at this point it is not a http url
	      (setq file (expand-file-name file base-href)))
            (let ((ext (file-name-extension file)))
              (when (member ext '("htm" "html"))
                (add-to-list 'possible (cons 'view-local (list file anchor)))))
	    (add-to-list 'possible (cons 'edit (list file anchor)))))))
    possible))


(defun nxhtml-href-click()
  "Action for href click.
Edit file referenced by href value. Position at anchor if present."
  (interactive)
  (let ((href-val (nxhtml-get-link-at (point) nxhtml-link-regexp)))
    (if href-val
	(let* ((possible (nxhtml-possible-href-actions href-val))
	       (edit (assoc 'edit possible))
	       (file (nth 1 edit))
	       (anchor (nth 2 edit))
	       )
	  (cond (edit
		 (nxhtml-edit-at file anchor))
		((assoc 'mailto possible)
                 (when (y-or-n-p "This is a mail address.  Do you want to send a message to this mail address? ")
		   (nxhtml-mail-to href-val)))
		((assoc 'view-web possible)
		 (when (y-or-n-p "Can't edit this URL, it is on the web.  View the URL in your web browser? ")
		   (browse-url href-val)))
		((assoc 'view-web-base possible)
		 (when (y-or-n-p "Can't edit, based URL is to the web.  View resulting URL in your web browser? ")
		   (browse-url (cdr view-web-base))))
		(t
		 (message "Do not know how to handle this URL"))
		))
      (message "No value for href attribute"))))


(defvar nxhtml-saved-link-file nil
  "Saved buffer file name for use in `nxhtml-paste-link'.")
(defvar nxhtml-saved-link-anchor nil
  "Saved anchor name for use in `nxhtml-paste-link'.")

;; Fix-me: same line???
(defun nxhtml-save-link-to-here()
  "Save buffer file name+anchor for `nxhtml-paste-link'."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer has no file name")
    (setq nxhtml-saved-link-file (buffer-file-name))
    (setq nxhtml-saved-link-anchor nil)
    (save-excursion
      (let ((here (point)))
        (while (not (or (bolp) (looking-at "\\(?:id\\|name\\) *= *\".*?\"")))
          (backward-char))
        (when (and (looking-at "\\(?:id\\|name\\) *= *\"\\(.*?\\)\"")
                   (<= (match-beginning 0) here)
                   (< here (match-end 0)))
          (setq nxhtml-saved-link-anchor (match-string-no-properties 1)))))
    (message "Saved link: %s%s" nxhtml-saved-link-file
             (if nxhtml-saved-link-anchor
                 (concat "#" nxhtml-saved-link-anchor)
               ""))))

(defun nxhtml-paste-link-as-a-tag()
  (interactive)
  "Paste link saved by `nxhtml-save-link-to-here' as an A tag.
Takes into account the relative position of the saved link."
  (interactive)
  (let ((paste-text (nxhtml-get-saved-link)))
    (when paste-text
      (let ((link-text (read-string "Link text: ")))
        (insert "<a href=\"" paste-text "\">" link-text "</a>")))))

(defun nxhtml-paste-link()
  "Paste link saved by `nxhtml-save-link-to-here'.
Takes into account the relative position of the saved link."
  (interactive)
  (let ((paste-text (nxhtml-get-saved-link)))
    (when paste-text
      (insert paste-text))))

(defun nxhtml-get-saved-link()
  (if nxhtml-saved-link-file
      (let* (
	     (base-href (nxhtml-find-base-href))
	     (rel (file-relative-name nxhtml-saved-link-file
				      (if base-href
					  base-href
					(file-name-directory (buffer-file-name)))))
	     (to-file (file-name-nondirectory (buffer-file-name)))
	     (anchor nxhtml-saved-link-anchor)
	     )
	(when (equal to-file rel) (setq rel ""))
	(when anchor (setq rel (concat rel "#" anchor)))
	rel)
    (message "There is no saved link")
    nil))


;;   (save-excursion
;;     (goto-char (point-min))
;;     (nxml-with-unmodifying-text-property-changes
;;     (nxml-do-fontify))))

(defun nxhtml-get-possible-attribute-names (lt-pos)
;;   (rng-set-state-after lt-pos)
;;   (and rng-collecting-text (rng-flush-text))
;;   (rng-match-possible-attribute-names))
  (let ((lt-pos (save-excursion (search-backward "<" nil t)))
        xmltok-dtd)
    (and lt-pos
         (= (rng-set-state-after lt-pos) lt-pos)
         (or (rng-complete-tag lt-pos)
             (rng-complete-end-tag lt-pos)
             (rng-complete-attribute-name lt-pos)
             ;;(rng-match-possible-attribute-names)
             ;;(rng-complete-attribute-value lt-pos)
             ))))

(defun nxhtml-get-possible-start-tag-names (lt-pos)
  "Get a list of all possible start-tag names."
  (rng-set-state-after lt-pos)
  (and rng-collecting-text (rng-flush-text))
  (rng-match-possible-start-tag-names))

(defun nxhtml-tag-completeable (tag lt-pos)
  (let ((possible-names (nxhtml-get-possible-start-tag-names lt-pos)))
    (when possible-names
      (let ((assoc (rassoc tag possible-names)))
        (when assoc
          (let* ((tname (cdr assoc))
                 (entered (buffer-substring-no-properties (1+ lt-pos) (point)))
                 (le (length entered))
                 (hit (and (<= le (length tname))
                           (string= entered (substring tname 0 le))))
                 )
            ;;(message "entered=%s, le=%s, tname=%s, ss=%s, hit=%s" entered le tname (substring tname 0 le) hit)(sit-for 1)
            (when hit (1+ le))))))))

(defun nxhtml-tag-insertable (tag)
  "Check if start tag is insertable at point.
Return number of already entered characters if so, including `<'.
Otherwise return nil."
  (if (eq major-mode 'nxhtml-mode)
      (progn
        ;;(message "tag-insertable here: %s" tag)(sit-for 2)
        (let (
              (lt-pos
               (save-excursion
                 (re-search-backward rng-in-start-tag-name-regex nil t)))
              )
          ;;(message "lt-pos=%s" lt-pos)(sit-for 1)
          (if lt-pos
              (nxhtml-tag-completeable tag lt-pos)
            (when (save-excursion (re-search-backward ">[^<]*\\=" nil t))
              (insert "<")
              ;;(dolist (f after-change-functions) (when (functionp f) (funcall f (1- (point)) (point) 0)))
              ;;(message "after <")(sit-for 1)
              (let ((ret (nxhtml-tag-completeable tag (1- (point)))))
                ;;(message "ret=%s" ret)(sit-for 1)
                ;;(undo)
                (primitive-undo 1 buffer-undo-list)
                ;;(message "after undo")(sit-for 1)
                ;;(dolist (f after-change-functions) (when (functionp f) (funcall f (point) (point) 1)))
                (when ret (1- ret)))))))
    t))


(defun nxhtml-point-to-coord(point)
  (let* ((pn (posn-at-point point))
	 (x-y (posn-x-y pn))
	 (x (car x-y))
	 (y (cdr x-y))
	 (pos (list (list x (+ y 20)) (selected-window)))
	 (ans)
	 )
    pos))

(defun nxhtml-href-menu()
  (let ((href-val (nxhtml-get-link-at
                   (point)
                   ;;"href"
                   nxhtml-link-regexp
                   ))
        (on-href (eq nxhtml-href-keymap (get-text-property (point) 'keymap))))
    ;;(message "href-val=%s" href-val)(sit-for 2)
    (when href-val
	(let* ((possible (nxhtml-possible-href-actions href-val))
	       (mailto (assoc 'mailto possible))
	       (view-web (assoc 'view-web possible))
	       (view-web-base (assoc 'view-web-base possible))
	       (edit (assoc 'edit possible))
	       (file (nth 1 edit))
	       (anchor (nth 2 edit))
	       (choices)
	       (answer)
	       )
          (let ((map (make-sparse-keymap "nxhtml-href-menu")))
            (when view-web
              (define-key map [nxhtm-href-view-web]
                (list 'menu-item "View Web Url"
                      `(lambda() (interactive)
                        (browse-url ,href-val)))))
            (when view-web-base
              (define-key map [nxhtm-href-view-web-based]
                (list 'menu-item "View Web Url (base URL found)"
                      `(lambda() (interactive)
                        (browse-url (cdr ,view-web-base))))))
            (when mailto
              (define-key map [nxhtml-href-mail]
                (list 'menu-item (concat "&Mail to " (substring href-val 7))
                      `(lambda() (interactive)
                        (nxhtml-mail-to ,href-val)))))
            (when edit
              (when (and (file-exists-p file)
                         (assoc 'view-local possible))
                (let ((url (concat "file:///" (expand-file-name file))))
                  (when anchor
                    (let ((url-anchor (concat url "#" anchor)))
                      (define-key map [nxhtml-href-view-file-at]
                        (list 'menu-item (concat "View File URL at #" anchor)
                              `(lambda() (interactive)
                                 (browse-url ,url-anchor))))))
                  (define-key map [nxhtml-href-view-file]
                    (list 'menu-item "&View File URL"
                          `(lambda() (interactive)
                             (browse-url ,url))))))
              (unless (equal file (buffer-file-name))
                (define-key map [nxhtml-href-edit]
                  (list 'menu-item "&Edit File"
                        `(lambda() (interactive)
                           (nxhtml-edit-at ,file nil)))))
              (when (and (file-exists-p file) anchor)
                (define-key map [nxhtml-href-edit-at]
                  (list 'menu-item (concat "Edit File &at #" anchor)
                        `(lambda() (interactive)
                           (nxhtml-edit-at ,file ,anchor))))))
            (define-key map [nxhtml-href-copy-link]
              (list 'menu-item "&Copy Link"
                    `(lambda() (interactive)
                      (x-select-text ,href-val))))
            map)))))


(defcustom nxhtml-default-encoding 'iso-8859-1
  "Default encoding."
  :type 'coding-system
  )

(defun nxhtml-insert-empty-frames-page()
  "Insert an empty frames page."
  (interactive)
  (unless (= 0 (buffer-size))
    (error "Buffer is not empty"))
  (insert
   "<?xml version=\"1.0\" encoding=\""
   (symbol-name nxhtml-default-encoding)
   "\"?>
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/xhtml1/DTD/xhtml1-frameset.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title></title>
  </head>
  <frameset cols=\"50%, 50%\">
    <frame src=\"about:blank\" />
    <frame src=\"about:blank\" />
  </frameset>
</html>")
  (search-backward "</title>"))

(defun nxhtml-insert-empty-page()
  "Insert an empty XHTML page."
  (interactive)
  (unless (= 0 (buffer-size))
    (error "Buffer is not empty"))
  (insert
   "<?xml version=\"1.0\" encoding=\""
   (symbol-name nxhtml-default-encoding)
   "\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title></title>
  </head>
  <body>
  </body>
</html>")
  (search-backward "</title>"))

(defun nxhtml-empty-page-completion()
  (unless (= 0 (buffer-size)) (error "Buffer is not empty"))
  (let* ((frames "Frameset page")
         (normal "Normal page")
         (hist (list normal frames))
         res)
    (setq res (completing-read "Insert empty page: " hist nil t normal (cons 'hist 1)))
    (cond ((string= res frames)
           (nxhtml-insert-empty-frames-page))
          ((string= res normal)
           (nxhtml-insert-empty-page))
          (t
           (error "Bad res=%s" res)))))


(defun nxhtml-edit-at(file &optional anchor)
  (find-file file)
  ;;(message "bs=%s" (buffer-size))(sleep-for 5)
  (when anchor
    (let ((here (point))
	  (anchor-regexp (concat "\\(?:id\\|name\\) *= *\"" anchor "\"")))
      (goto-char (point-min))
      (unless (search-forward-regexp anchor-regexp nil t)
	(message "Anchor \"%s\" not found" anchor)
	(goto-char here)))))

(defun nxhtml-mail-to(addr)
  (cond ((fboundp 'w32-shell-execute)
	 (w32-shell-execute "open" href-val))
	(t (message "Don't know how to how to start mail"))))


(defun nxhtml-add-appmenu()
  "Add entries for nXHtml to AppMenu.
The entries are added to `appmenu-modes-alist'.

This is done only if appmenu.el is loaded."
  (when (featurep 'appmenu)
    (add-to-list 'appmenu-modes-alist
                 (cons 'nxhtml-mode
                       (cons "Current Link" 'nxhtml-href-menu)))))



(defvar nxhtml-mode-hook nil)
(add-hook 'nxhtml-mode-hook 'nxml-fontify-buffer)
;;(add-hook 'nxhtml-mode-hook 'nxhtml-insert-skeleton-if-empty)

(defun nxhtml-help()
  (interactive)
  (describe-function 'nxhtml-mode))

(defun nxhtml-xmlpe-mode()
  (interactive)
  (if (and (featurep 'xmlpe) xmlpe-mode)
      (xmlpe-mode 0)
    (if (or (not rng-validate-mode)
            (< 0 rng-error-count))
        (xmlpe-mode 1)
      (when (let ((use-dialog-box nil))
              (y-or-n-p "This seems to be a full XHTML file. Sure you want to edit as XHTML fragment? "))
        (xmlpe-mode 1)))))

(defun nxhtml-view-file()
  "View file."
  (interactive)
  (if (and (featurep 'xmlpe) xmlpe-mode)
      (xmlpe-view)
    (browse-url-of-file)))

(defun nxhtml-buffer-file-possibly-viewable()
  (and buffer-file-name
       (not (and (featurep 'xmlpe) xmlpe-mode))))

(defconst nxhtml-mode-menu-map
  (let ((map (make-sparse-keymap "nxhtml-mode-menu")))

;;     (when (featurep 'tidy)
;;       ;;(tidy-build-menu map) ;; Does not work for me??
;;       (tidy-parse-config-file)
;;       (define-key map [nxhtml-tidy-menu]
;;         (list 'menu-item "Tidy abcdef" tidy-menu))
;;       )

    (let ((help-map (make-sparse-keymap)))
      (define-key help-map [nxhtml-features-check]
        (list 'menu-item "Check Wanted Features" 'nxhtml-features-check))
      (define-key help-map [nxhtml-help]
        (list 'menu-item "Nxhtml Help" 'nxhtml-help))
      (define-key map [nxhtml-help-map]
        (list 'menu-item "Nxhtml Info" help-map))
      )

;;     (when (featurep 'appmenu)
;;       (when (featurep 'xhtml-help)
;;         (let ((menu-bar-entry (lookup-key xhtml-help-mode-keymap [menu-bar])))
;;           (when menu-bar-entry
;;             (map-keymap
;;              (lambda(key command)
;;                (define-key map
;;                  (vector (intern (concat "nxhtml-" (symbol-name key))))
;;                  command)
;;                )
;;              menu-bar-entry)
;;             (define-key map [nxhtml-xhtml-help-separator] (list 'menu-item "--"))
;;             )))
;;       )

    (when (featurep 'xhtml-help)
      (let ((xhelp-map (make-sparse-keymap)))
        (define-key map [nxhtml-xhelp-map]
          (list 'menu-item "Tags and Attributes Help" xhelp-map))
        (define-key xhelp-map [nxhtml-css-help]
          (list 'menu-item "CSS Help" 'xhtml-help-show-css-ref))
        (define-key xhelp-map [nxhtml-tag-help]
          (list 'menu-item "XHTML Tag Help" 'nxhtml-short-tag-help))))

    (when (featurep 'hexcolor)
      (let ((hexclr-map (make-sparse-keymap)))
        (define-key map [nxhtml-hexcolor]
          (list 'menu-item "Color Help" hexclr-map))
        (define-key hexclr-map [nxhtml-hexcolor-mode]
          (list 'menu-item "Hex Color Mode" 'hexcolor-mode
                :enable 'font-lock-syntax-table
                :button '(:toggle . hexcolor-mode)))
        (define-key hexclr-map [nxhtml-hexcolor-test]
          (list 'menu-item "Color Test" 'hexcolor-test))))

    (define-key map [nxhtml-help-separator] (list 'menu-item "--"))


    (when (featurep 'xmlpe)
      (define-key map [nxhtml-xmlpe]
        (list 'menu-item "Edit as XML Fragment" 'nxhtml-xmlpe-mode
              :button '(:toggle . xmlpe-mode)))
      )
    (when (featurep 'html-inlined)
      (define-key map [nxhtml-inlined]
        (list 'menu-item "Edit Inlined Code" 'html-inlined-narrow))
      )
    (when (or (featurep 'xmlpe)
              (featurep 'html-inlined))
      (define-key map [nxhtml-pe-separator] (list 'menu-item "--"))
      )


    (define-key map [nxhtml-finish-element]
      (list 'menu-item "Insert end tag" 'nxml-finish-element))
    (define-key map [nxhtml-complete]
      (list 'menu-item "Complete and insert" 'nxhtml-complete-and-insert))

    (define-key map [nxhtml-insert-separator] (list 'menu-item "--"))


    (when (featurep 'html-upl)
      (let ((upl-map (make-sparse-keymap)))
        (define-key map [nxhtml-upl-map]
          (list 'menu-item "File Transfer" upl-map))
        (define-key upl-map [nxhtml-upl-edit-remote-wtoc]
          (list 'menu-item "Edit Remote File With TOC" 'html-upl-edit-remote-file-with-toc))
        (define-key upl-map [nxhtml-upl-edit-remote]
          (list 'menu-item "Edit Remote File" 'html-upl-edit-remote-file))
        (define-key upl-map [nxhtml-upl-sep] (list 'menu-item "--"))
        (define-key upl-map [nxhtml-upl-upload-file]
          (list 'menu-item "Upload Single File" 'html-upl-upload-file))
        (define-key upl-map [nxhtml-upl-upload-site-with-toc]
          (list 'menu-item "Upload Site with TOC" 'html-upl-upload-site-with-toc))
        (define-key upl-map [nxhtml-upl-upload-site]
          (list 'menu-item "Upload Site" 'html-upl-upload-site))
      ))


    (let ((view-map (make-sparse-keymap)))
      (define-key map [nxhtml-view-map]
        (list 'menu-item "View" view-map))
      (when (featurep 'xmlpe)
        (define-key view-map [nxhtml-view-region]
          (list 'menu-item "View the Region Only" 'xmlpe-view-region
                :enable '(mark)))
        (define-key view-map [nxhtml-upl-sep3] (list 'menu-item "--")))
      (when (featurep 'html-upl)
        (define-key view-map [nxhtml-upl-view-remote-wtoc]
          (list 'menu-item "View Uploaded File With TOC" 'html-upl-view-remote-with-toc
                :enable '(nxhtml-buffer-file-possibly-viewable)))
        (define-key view-map [nxhtml-upl-view-remote-frame-file]
          (list 'menu-item "View Uploaded Frames File" 'html-upl-view-remote-frames
                :enable '(nxhtml-buffer-file-possibly-viewable)))
        (define-key view-map [nxhtml-upl-view-remote]
          (list 'menu-item "View Uploaded File" 'html-upl-view-remote
                :enable '(nxhtml-buffer-file-possibly-viewable)))
        (define-key view-map [nxhtml-upl-sep2] (list 'menu-item "--")))
      (when (featurep 'html-wtoc)
        (define-key view-map [nxhtml-view-merged-file]
          (list 'menu-item "View File With TOC" 'html-wtoc-view-page-with-toc
                :enable '(nxhtml-buffer-file-possibly-viewable))))
      (when (featurep 'html-toc)
        (define-key view-map [nxhtml-view-frame-file]
          (list 'menu-item "View Frames File" 'html-toc-view-frames-file
                :enable '(nxhtml-buffer-file-possibly-viewable))))
      (define-key view-map [nxhtml-view-file]
        (list 'menu-item "View File" 'nxhtml-view-file
              :enable 'buffer-file-name))
      )
;;     (when (featurep 'fold-dwim)
;;       (let ((fd-map (make-sparse-keymap)))
;;         (define-key fd-map [fold-dwim-toggle]
;;           (list 'menu-item "Fold Dwin Toggle" 'fold-dwim-toggle))
;;         (define-key fd-map [fold-dwim-hide-all]
;;           (list 'menu-item "Fold Dwin Hide All" 'fold-dwim-hide-all))
;;         (define-key fd-map [fold-dwim-show-all]
;;           (list 'menu-item "Fold Dwin Show All" 'fold-dwim-show-all))
;;         (define-key map [fold-dwim]
;;           (list 'menu-item "Folding" fd-map))))
;;     (when (or (featurep 'fold-dwim)
;;               (featurep 'html-pagetoc))
;;       (define-key map [nxhtml-html-pagetoc-separator] (list 'menu-item "--")))


    (let ((link-map (make-sparse-keymap)))

      (when (featurep 'html-chklnk)
        (define-key link-map [nxhtml-chklnk]
          (list 'menu-item "Check Links" 'html-chklnk-check-site-links)))

      (when (featurep 'html-move)
        (let ((move-map (make-sparse-keymap)))
          (define-key move-map [html-move-buffer-file]
            (list 'menu-item "Move Buffer File" 'html-move-buffer-file
                  :help "Move buffer file and update links"
                  :enable 'buffer-file-name))
          (define-key link-map [move-map]
            (list 'menu-item "Moving Files" move-map))
          (define-key link-map [move-map-separator] (list 'menu-item "--"))
          ))


      (define-key link-map [nxhtml-paste-link]
        (list 'menu-item "Paste Saved Relative Link" 'nxhtml-paste-link
              :help "Paste link"
              :enable 'nxhtml-saved-link-file))
      (define-key link-map [nxhtml-paste-link-as-a-tag]
        (list 'menu-item "Paste Saved Relative Link as <a href=...>" 'nxhtml-paste-link-as-a-tag
              :help "Paste link as <a ...> tag"
              :enable 'nxhtml-saved-link-file))
      (define-key link-map [nxhtml-save-link-to-here]
        (list 'menu-item "Save Relative Link to Current File" 'nxhtml-save-link-to-here
              :help "Save link info for current file"
              :enable 'buffer-file-name))

      (define-key link-map [nxhtml-separator-1] (list 'menu-item "--"))
      (define-key link-map [nxhtml-next-href]
        (list 'menu-item "Next Link" 'nxhtml-next-href
              :help "Go to next href field"))
      (define-key link-map [nxhtml-prev-href]
        (list 'menu-item "Previous Link" 'nxhtml-prev-href
              :help "Go to previous href field"))

      (when (featurep 'xhtmlin)
        (let ((menu-bar-entry (lookup-key xhtmlin-mode-map [menu-bar])))
          (when menu-bar-entry
            (define-key link-map [nxhtml-xhtmlin-separator] (list 'menu-item "--"))
            (map-keymap
             (lambda(key command)
               (define-key link-map
                 (vector (intern (concat "nxhtml-" (symbol-name key))))
                 command))
             menu-bar-entry)
            ))

        )

      (define-key map [nxhtml-link-map]
        (list 'menu-item "Links" link-map)))


    (when (or (featurep 'html-pagetoc)
              (featurep 'html-toc))
      (let ((sometoc-map (make-sparse-keymap)))
        (when (featurep 'html-toc)
          (let ((toc-map (make-sparse-keymap)))
            (define-key sometoc-map [nxhtml-toc-map]
              (list 'menu-item "For Site" toc-map))
            (when (featurep 'html-wtoc)
              (define-key toc-map [nxhtml-html-wtoc]
                (list 'menu-item "Merge Pages and TOC" 'html-wtoc-write-pages-with-toc)))
            (define-key toc-map [nxthml-html-toc]
              (list 'menu-item "With Frames" html-toc-menu-map))))
        (when (featurep 'html-pagetoc)
          ;;(when (featurep 'html-toc)
          ;;  (define-key sometoc-map [nxhtml-pagetoc-sep] (list 'menu-item "--")))
          (define-key sometoc-map [nxhtml-html-pagetoc]
            (list 'menu-item "For Page" html-pagetoc-menu-map)))
        (define-key map [nxhtml-sometoc-map]
          (list 'menu-item "Table of Contents" sometoc-map))))

    (let ((site-map (make-sparse-keymap)))
      (define-key map [nxhtml-site-map]
        (list 'menu-item "Site" site-map))
      (define-key site-map [nxhtml-customize-site-list]
        (list 'menu-item "Edit Sites" (lambda() (interactive)
                                        (customize-option 'html-site-list))))
      (define-key site-map [nxhtml-set-site]
        (list 'menu-item "Set Current Site" 'html-site-set-site))
      )
    map))


;; Unfortunately this does not work as I thought since Emacs
;; computes the whole menu every time the menu is touched
;; (2006-02-24).
;; (defun nxhtml-make-insert-sub(real-menu)
;;   (message "insert-sub")
;;   (let ((ins-map (make-sparse-keymap)))
;;     (define-key ins-map [nxhtml-ins-css]
;;       (list 'menu-item "Insert CSS Link" 'nxhtml-insert-css
;;             :enable (nxhtml-tag-insertable "link")))
;;     (define-key ins-map [nxhtml-ins-js]
;;       (list 'menu-item "Insert Javascript Link" 'nxhtml-insert-javascript
;;             :enable (nxhtml-tag-insertable "link")))
;;     (define-key ins-map [nxhtml-ins-img]
;;       (list 'menu-item "Insert Img Tag" 'nxhtml-insert-img
;;             :enable (nxhtml-tag-insertable "img")))
;;     (define-key ins-map [nxhtml-ins-a]
;;       (list 'menu-item "Insert A Link Tag" 'nxhtml-insert-a
;;             :enable (nxhtml-tag-insertable "a")))
;;     ins-map)
;;    ;;(list ["testing" nxhtml-insert-css :active t] )
;;   )

(defvar nxhtml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?h) ?c]    'nxhtml-save-link-to-here)
    (define-key map [(control ?c) (control ?h) ?v]    'nxhtml-paste-link-as-a-tag)
    (define-key map [(control ?c) (control ?h) ?b ?f] 'nxhtml-view-file)
    (define-key map [(control ?c) (control ?h) ?b ?b] 'browse-url-of-buffer)
    (define-key map [(control ?c) tab]         'nxhtml-next-href)
    (define-key map [(control ?c) (shift tab)] 'nxhtml-prev-href)
    (define-key map [(control ret)] 'nxhtml-complete-and-insert)
    (define-key map [(meta tab)] 'nxhtml-complete-and-insert)
    (when (featurep 'xhtmlin)
;;       (define-key map [(control ?c) (control ?h) ?i ?j] 'nxhtml-insert-javascript)
;;       (define-key map [(control ?c) (control ?h) ?i ?i] 'nxhtml-insert-img)
;;       (define-key map [(control ?c) (control ?h) ?i ?c] 'nxhtml-insert-css)
;;       (define-key map [(control ?c) (control ?h) ?i ?a] 'nxhtml-insert-a)
      )
    (when (featurep 'html-pagetoc)
      (define-key map [(control ?c) (control ?h) ?i ?t ?t] 'html-pagetoc-insert-toc)
      (define-key map [(control ?c) (control ?h) ?i ?t ?r] 'html-pagetoc-rebuild-toc)
      (define-key map [(control ?c) (control ?h) ?i ?t ?s] 'html-pagetoc-insert-style-guide)
      )
    (when (featurep 'xhtml-help)
      ;;(define-key map [(control ?c) (f1) ?x] 'xhtml-help-show-tag-ref)
      (define-key map [(control ?c) (f1) ?x] 'nxhtml-short-tag-help)
      (define-key map [(control ?c) (f1) ?c] 'xhtml-help-show-css-ref)
      )
    (define-key map [menu-bar nxhtml-mode]
      (list 'menu-item "XHTML" nxhtml-mode-menu-map))
    map))

(eval-after-load 'css-mode
  '(when (featurep 'xhtml-help)
    (define-key css-mode-map [(control ?c) (f1) ?c] 'xhtml-help-show-css-ref)
    ))

(define-derived-mode nxhtml-mode nxml-mode "nXhtml"
  "Major mode for editing XHTML files.  Based on `nxml-mode'.
This mode adds some features that are useful when editing XHTML
files to `nxml-mode':

- Link handling:
  - Easier insertion of tags with links.
  - Following links to edit or view.
  - Moving between links.
  - Moving files and automatically update affected links.
  - Copy link to id location and paste it back as a relative link.
  - Link checking (local links only)
- Creating table of contents for a page.
- Creating table of contents for a site.
- Merging of pages and table of contents for a site.
- Edit a fragment of an XHTML file (for blogging for example)
- Edit of inlined CSS and JavaScript.
- Support for folding
- Help for XHTML tags and CSS attributes.
- Uploading
- Editing of remote files
- Easy viewing of local and remote files
- Adding a popup menu to [apps] to access some of these features.

Some of these features are optional and available only if other
Emacs modules are found.  Use M-x `nxhtml-features-check' to
get a list of these features and modules needed.

Href attributes will be underlined.  A special keymap is bound to
the underlined area:

  Tab, S-Tab    Move between underlined hrefs
  RET, Mouse-1  Follow link inside Emacs (if possible)

If the link is not into a file you can edit (a mailto link for
example) you will be prompted for an alternative action.

If you are on an id or name attribute C-c c will copy the link.
C-c v will paste this as an a-tag."
  ;;:init-value nil
  ;;:lighter " XH"
  ;;:keymap nxhtml-mode-map
  (nxhtml-add-appmenu)
  ;;(define-key nxhtml-mode-map [(meta tab)] 'nxml-complete)
  (when (featurep 'fold-dwim)
    (nxhtml-setup-for-fold-dwim)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below I have taken some seeds and trees from html-helper-mode and
;; planted them here.

;; html-helper-mode has a concept of "type" of tags. Each type is a
;; list of tags that all go together in one keymap and one menu.
;; Types can be added to the system after html-helper has been loaded,
;; briefly by doing html-helper-add-type-to-alist, then
;; html-helper-install-type, then html-helper-add-tag (for each tag)
;; then html-helper-rebuild-menu. See the mode documentation for more detail.

(require 'tempo)

(defconst html-helper-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `html-helper-add-type-to-alist'.")

(defun html-helper-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (html-helper-menu-string-for type)
	(eval (html-helper-menu-for type))))



(defconst nxhtml-helper-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `nxhtml-helper-add-type-to-alist'.")

(defun nxhtml-helper-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq nxhtml-helper-type-alist (cons type nxhtml-helper-type-alist)))

;; Here are the types provided by html-helper-mode.
(mapcar 'nxhtml-helper-add-type-to-alist
  '((entity  . (nil nil html-helper-entity-menu "Insert Character Entities"))
    (textel  . (nil nil html-helper-textel-menu "Insert Text Elements"))
    (head    . (html-helper-head-map "\C-c\C-h"
				     html-helper-head-menu
				     "Insert Structural Elements"))
    (header  . (html-helper-header-map "\C-c\M-h"
				       html-helper-header-menu
				       "Insert Headers"))
    (anchor  . (html-helper-anchor-map "\C-c\C-a"
				       html-helper-anchor-menu
				       "Insert Hyperlinks"))
    (logical . (html-helper-logical-map "\C-c\M-l"
					html-helper-logical-menu
					"Insert Logical Styles"))
    (phys    . (html-helper-phys-map "\C-c\C-p"
				     html-helper-phys-menu
				     "Insert Physical Styles"))
    (list    . (html-helper-list-map "\C-c\C-l"
				     html-helper-list-menu
				     "Insert List Elements"))
    (form    . (html-helper-form-map "\C-c\C-f"
				     html-helper-form-menu
				     "Insert Form Elements"))
    (image   . (html-helper-image-map "\C-c\C-i"
				      html-helper-image-menu
				      "Insert Inlined Images"))
    (table   . (html-helper-table-map "\C-c\C-t"
				      html-helper-table-menu
				      "Insert Tables"))
    (script  . (html-helper-script-map "\C-c\C-s"
				       html-helper-script-menu
				       "Insert Scripts"))
    ))

;; Once html-helper-mde is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst html-helper-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

;;{{{ html-helper-add-tag function for building basic tags

(defvar html-helper-tempo-tags nil
  "List of tags used in completion.")

;; this while loop is awfully Cish
;; isn't there an emacs lisp function to do this?
(defun html-helper-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
	 (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
	  (aset s l ?\-))
      (setq l (1- l)))
    (concat "html-" (downcase s))))


(defun html-helper-add-tag (l)
  "Add a new tag to html-helper-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(html-helper-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
	 (keymap (html-helper-keymap-for type))
	 (menu (html-helper-menu-for type))
	 (key (nth 1 l))
	 (completer (nth 2 l))
	 (name (nth 3 l))
	 (tag (nth 4 l))
	 (doc (nth 5 l))
	 (command (if (string-equal completer "function")
		      (nth 4 l)
                    (tempo-define-template (html-helper-string-to-symbol name)
                                           tag completer doc
                                           'html-helper-tempo-tags))))

    (if (null (memq type html-helper-installed-types))    ;type loaded?
	t                                                 ;no, do nothing.
      (if (stringp key)			                  ;bind key somewhere?
	  (if keymap			                  ;special keymap?
	      (define-key (eval keymap) key command)      ;t:   bind to prefix
	    (define-key html-helper-mode-map key command));nil: bind to global
	t)
      (if menu				                  ;is there a menu?
	  (set menu			                  ;good, cons it in
	       (cons (vector name command t) (eval menu))))
      )))

;; for backwards compatability
(fset 'html-helper-add-cookie 'html-helper-add-tag)

;;}}}
;; These tags are an attempt to be HTML/2.0 compliant, with the exception
;; of container <p>, <li>, <dd>, <dt> (we adopt 3.0 behaviour).
;; For reference see <URL:http://www.w3.org/hypertext/WWW/MarkUp/MarkUp.html>

;; order here is significant: within a tag type, menus and mode help
;; go in the reverse order of what you see here. Sorry about that, it's
;; not easy to fix.

(mapcar
 'html-helper-add-tag
 '(
   ;;entities
   (entity  "\C-c#"   "&#"              "Ascii Code"      ("&#" (r "Ascii: ") ";"))
   (entity  "\C-c\""  "&quot;"          "Quotation mark"  ("&quot;"))
   (entity  "\C-c$"   "&reg;"           "Registered"      ("&reg;"))
   (entity  "\C-c@"   "&copy;"          "Copyright"       ("&copy;"))
   (entity  "\C-c-"   "&shy;"           "Soft Hyphen"     ("&shy;"))
   (entity  "\C-c "   "&nbsp;"		"Nonbreaking Space"  ("&nbsp;"))
   (entity  "\C-c&"   "&amp;"		"Ampersand"	  ("&amp;"))
   (entity  "\C-c>"   "&gt;"	  	"Greater Than"       ("&gt;"))
   (entity  "\C-c<"   "&lt;"		"Less Than"	  ("&lt;"))
   ;; letters with accents common in italian
   (entity  "\C-ca"   "&agrave;"        "a` (&&agrave;)"          ("&agrave;"))
   (entity  "\C-ce"   "&egrave;"        "e` (&&egrave;)"          ("&egrave;"))
   (entity  "\C-cE"   "&eacute;"        "e' (&&eacute;)"          ("&eacute;"))
   (entity  "\C-co"   "&ograve;"        "o` (&&ograve;)"          ("&ograve;"))
   (entity  "\C-ci"   "&igrave;"        "i` (&&igrave;)"          ("&igrave;"))
   (entity  "\C-cu"   "&ugrave;"        "u` (&&ugrave;)"          ("&ugrave;"))

   ;; logical styles
   (logical "b"       "<blockquote>"	"Blockquote"
	    ("<blockquote>" (r "Quote: ") "</blockquote>"))
   (logical "c"       "<code>"		"Code"
	    ("<code>" (r "Code: ") "</code>"))
   (logical "x"       "<samp>"		"Sample"
	    ("<samp>" (r "Sample code") "</samp>"))
   (logical "r"       "<cite>"		"Citation"
	    ("<cite>" (r "Citation: ") "</cite>"))
   (logical "k"       "<kbd>"		"Keyboard Input"
	    ("<kbd>" (r "Keyboard: ") "</kbd>"))
   (logical "v"       "<var>"		"Variable"
	    ("<var>" (r "Variable: ") "</var>"))
   (logical "d"       "<dfn>"		"Definition"
	    ("<dfn>" (r "Definition: ") "</dfn>"))
   (logical "a"	      "<address>"	"Address"
	    ("<address>" r "</address>"))
   (logical "e"       "<em>"		"Emphasized"
	    ("<em>" (r "Text: ") "</em>"))
   (logical "s"       "<strong>"	"Strong"
	    ("<strong>" (r "Text: ") "</strong>"))
   (logical "p"       "<pre>"		"Preformatted"
	    ("<pre>" (r "Text: ") "</pre>"))

   ;;physical styles
   (phys    "s"       "<strike>"	"Strikethru"
	    ("<strike>" (r "Text: ") "</strike>"))
   (phys    "u"       "<u>"		"Underline"
	    ("<u>" (r "Text: ") "</u>"))
   (phys    "i"       "<i>"		"Italic"
	    ("<i>" (r "Text: ") "</i>"))
   (phys    "b"	      "<b>"    		"Bold"
	    ("<b>" (r "Text: ") "</b>"))
   (phys    "f"       "<tt>"		"Fixed"
	    ("<tt>" (r "Text: ") "</tt>"))
   (phys    "c"       "<center>"        "Center"
	    ("<center>" (r "Text: ") "</center>"))

;; html4.0 stuff, omitted

;    (phys    "5" "<span style=" "Spanning style"
; 	     ("<span style=\"" (p "style: ") "\">" 'r "</span>"))
;    (phys    "l" "<span class=" "Spanning class"
; 	     ("<span class=\"" (p "class: ") "\">" 'r "</span>"))


   ;;headers
   (header  "6"       "<h6>"		"Header 6"
	    ("<h6>" (r "Header: ") "</h6>"))
   (header  "5"       "<h5>"		"Header 5"
	    ("<h5>" (r "Header: ") "</h5>"))
   (header  "4"       "<h4>"		"Header 4"
	    ("<h4>" (r "Header: ") "</h4>"))
   (header  "3"       "<h3>"		"Header 3"
	    ("<h3>" (r "Header: ") "</h3>"))
   (header  "2"       "<h2>"		"Header 2"
	    ("<h2>" (r "Header: ") "</h2>"))
   (header  "1"	      "<h1>"     	"Header 1"
	    ("<h1>" (r "Header: ") "</h1>"))

   ;; forms
   (form    "o"       "<option>"        "Option"
	    (& "<option>" > ))
   (form    "v"       "<option value"   "Option with Value"
	    (& "<option value=\"" (r "Value: ") "\">" >))
   (form    "s"       "<select"		"Selections"
	    ("<select"
	     (html-helper-insert-or-wipe "name") ">\n<option>" > "\n</select>")"<select")
   (form    "z"	      "<input"		"Reset Form"
	    ("<input type=\"RESET\""
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "m"	      "<input"		"Submit Form"
	    ("<input type=\"SUBMIT\""
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "b"       "<input"          "Button"
	    ("<input type=\"BUTTON\""
	     (html-helper-insert-or-wipe "value")
	     (html-helper-insert-or-wipe "name") ">"))
   (form    "i"	      "<input"		"Image Field"
	    ("<input type=\"IMAGE\""
	     (html-helper-insert-or-wipe "Name")
	     (html-helper-insert-or-wipe "src") ">"))
   (form    "h"       "<input"          "Hidden Field"
	    ("<input type=\"HIDDEN\""
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "p"	      "<textarea"	"Text Area"
	    ("<textarea"
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "rows")
	     (html-helper-insert-or-wipe "cols") ">" r "</textarea>"))
   (form    "c"	      "<input"		"Checkbox"
	    ("<input type=\"CHECKBOX\""
	     (html-helper-insert-or-wipe "name") ">"))
   (form    "r"	      "<input"		"Radiobutton"
	    ("<input type=\"RADIO\""
	     (html-helper-insert-or-wipe "Name") ">"))
   (form    "t"	      "<input"		"Text Field"
	    ("<input type=\"TEXT\""
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "size") ">"))
   (form    "f"	      "<form"           "Form"
	    ("<form"
	     (html-helper-insert-or-wipe "action")
	     (html-helper-insert-or-wipe "method") ">\n</form>\n"))

   ;;lists
   (list    "t"       "<dt>"            "Definition Item"
	    (& "<dt>" > (r "Term: ") "\n<dd>" >
	       (r "Definition: ")))
   (list    "l"       "<li>"            "List Item"
	    (& "<li>" > (r "Item: ") > "</li>"))
   (list    "r"	      "<dir>"		"DirectoryList"
	    (& "<dir>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</dir>" >))
   (list    "m"	      "<menu>"		"Menu List"
	    (& "<menu>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</menu>" >))
   (list    "o"	      "<ol>"		"Ordered List"
	    (& "<ol>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</ol>" >))
   (list    "d"	      "<dl>"		"Definition List"
	    (& "<dl>" > "\n<dt>" >
	       (p "Term: ") "\n<dd>" >
	       (r "Definition: ") "\n</dl>" >))
   (list    "u"	      "<ul>"		"Unordered List"
	    (& "<ul>" > "\n<li>" > (r "Item: ") "\n</li>\n</ul>" >))

   ;;anchors
   (anchor  "n"	      "<a name="	"Link Target"
	    ("<a " (html-helper-insert-or-wipe "name") ">"
	     (r "Anchor text: ") "</a>"))
   (anchor  "l"	      "<a href="        "Hyperlink"
	    ("<a href=\"" (p "href: ") "\" >"
	     (r "Anchor text: ") "</a>"))

   ;;graphics
;    (image   "a"       nil               "Aligned Image"
; 	    ("<img align=\""
; 	     (r "Alignment: ") "\" src=\"" (r "Image URL: ") "\">"))
;    (image   "i"       "<img src="	"Image"
; 	    ("<img src=\""
; 	     (r "Image URL: ") "\">"))
;    (image   "e"       "<img align="     "Aligned Image With Alt. Text"
; 	    ("<img align=\""
; 	     (r "Alignment: ") "\" src=\""
; 	     (r "Image URL: ") "\" alt=\""
; 	     (r "Text URL: ") "\">"))
;    (image   "t"       "<img alt="	"Image With Alternate Text"
; 	    ("<img alt=\""
; 	     (r "Text URL: ") "\" src=\""
; 	     (r "Image URL: ") "\">"))
;; New, (almost) all including, single menu item entry
;; src has to be there!
   (image	"a"	nil	"Image"
		("<img src=\""
		 (p "src" ) "\" "
		 (html-helper-insert-or-wipe  "alt" )
		 (html-helper-insert-or-wipe  "height" )
		 (html-helper-insert-or-wipe  "width" )
		 (html-helper-insert-or-wipe  "align" )
		 ">"))
   ;; table
   (table   "t"       "<table>"         "Table"
	    ("<table"
	     (html-helper-insert-or-wipe  "border" )
	     (html-helper-insert-or-wipe "width" )">\n</table>"))
   (table   "r"       "<tr>"         "Table Row"
	    ("<TR>\n</TR>"))
   (table   "h"       "<th>"         "Table Header"
	    ("<TH"
	     (html-helper-insert-or-wipe "rowspan" )
	     (html-helper-insert-or-wipe "colspan")">\n</TH>"))
   (table   "d"       "<td>"         "Table Data"
	    ("<TD"
	     (html-helper-insert-or-wipe "align" )
	     (html-helper-insert-or-wipe "colspan")"></TD>"))
   (table   "p"	"<caption>"	"html table caption"
	    ("<caption>" (r . "Table: ") "</caption>"))
   ;;text elements
   (textel  "\C-c="    nil		"Horizontal Line"
	    (& "<hr>\n"))
   (textel  "\C-c\C-m" nil		"Line Break"
	    ("<br>\n"))
   (textel  "\e\C-m"  nil		"Paragraph"
	    ("<p>"
	     (r "Text: ") "</p>"))

   ;;head elements
   (head    "H"       "<head>"          "Head"
	    ("<head>\n" "</head>\n"))
   (head    "B"       "<body>"          "Body"
	    ("<body>\n" "</body>\n"))
   (head    "i"	      "<isindex>"	"Isindex"
	    ("<isindex>\n"))
   (head    "n"	      "<nextid>"	"Nextid"
	    ("<nextid>\n"))
   (head    "h"       "<meta http-equiv=" "HTTP Equivalent"
	    ("<meta"
	     (html-helper-insert-or-wipe "http-equiv") " content=\""
	     (r "Content: ") "\">\n"))
   (head    "m"       "<meta name="     "Meta Name"
	    ("<meta"
	     (html-helper-insert-or-wipe "name") " content=\""
	     (r "Content: ") "\">\n"))
   (head    "l"	      "<link"		"Link"
	    ("<link href=\"" p "\">"))
   (head    "b"       "<base"		"Base"
	    ("<base href=\"" r "\">"))
   (head    "t"	      "<title>"		"Title"
	    ("<title>" (r "Document title: ") "</title>"))
   ;; scripting elements
   (script  "j"    "<SCRIPT>"       "JavaScript"
	    ("<SCRIPT TYPE=\"text/javascript\">\n"
	     (r "Script") "</SCRIPT>"))
   (script  "v"    "<SCRIPT>"       "VBScript"
	    ("<SCRIPT TYPE=\"text/vbscript\">\n"
	     (r "Script") "</SCRIPT>"))
   (script  "%"    "<%="            "ASP output"
	    ("<%="(p " Variabile: ")"%>"))
   (script  "a"    "<%xx%>"         "JSP/ASP code"
	    ("<%\n"(r "Code: " )"\n%>"))
   (script  "<"    "<%xx%>"         "JSP/ASP break"
	    ("%>\n"(r "Code: " )"\n<%"))
   (script  "="    "<?="            "PHP output"
	    ("<?="(p " Variabile: ")"?>"))
   (script  "p"    "<?xx?>"         "PHP code"
	    ("<? PHP\n"(r "Code: " )"\n?>"))
   (script  "?"    "<?xx?>"         "PHP break"
	    ("?>\n"(r " Code: " )"\n<? PHP"))
   ))

;; Once html-helper-mde is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace some functions to allow more help on completion. The
;; functions should work as before in other modes than nxhtml-mode.

;; Some tempo tests - preparing to include html-helper-mode code:
(require 'tempo)
(tempo-define-template "hyper"
                        '("<a href=\""
                          ;;(progn (sit-for 0.1) (read-from-minibuffer "Link to: "))
;;                           (progn
;;                             (rng-after-change-function (point-min) (point) (- (point) (point-min)))
;;                             (nxml-after-change (point-min) (point) (- (point) (point-min)))
;;                             (rng-validate-while-idle (current-buffer))
;;                             ;;(and rng-collecting-text (rng-flush-text))
;;                             (nxhtml-read-url))
                          (nxhtml-read-url)
                          "\">"
                          (r "Link text: ")
                          "</a>"))

(defadvice tempo-insert (around tempo-insert-nxhtml-ad
                                nil
                                activate)
  (let ((start-point (point)))
    ad-do-it
    (when (eq major-mode 'nxhtml-mode)
      (rng-after-change-function start-point (point) 0)
      (nxml-after-change start-point (point) 0)
      (rng-validate-while-idle (current-buffer)))))

(defvar nxhtml-single-tags-hash
  (let ((h (make-hash-table :test 'equal)))
    (puthash "base"     "base -- Base URL/target" h)
    (puthash "meta"     "meta -- Meta information" h)
    (puthash "link"     "link -- Style sheet link" h)
    (puthash "br"       "br -- Line break" h)
    (puthash "hr"       "hr -- Horizontal rule" h)
    (puthash "frame"    "frame -- Sub window" h)
    (puthash "img"      "img -- Image" h)
    (puthash "input"    "input -- Input field/checkbox etc" h)
    (puthash "option"   "option -- Option in drop down list" h)
    (puthash "param"    "param -- Object settings" h)
    h))

(defvar nxhtml-completion-hash
  (let ((h (make-hash-table :test 'equal)))
    (puthash "html"     "html -- Document" h)
    (puthash "head"     "head -- Document head" h)
    (puthash "title"    "title -- Document or element title" h)
    (puthash "base"     "base -- Base URL/target" h)
    (puthash "meta"     "meta -- Meta information" h)
    (puthash "style"    "style -- Inline style sheet" h)
    (puthash "link"     "link -- Style sheet link" h)
    (puthash "script"   "script -- (Java)Script code" h)
    (puthash "noscript" "noscript -- Script disabled code" h)

    (puthash "iframe"   "iframe -- Inline frame" h)
    (puthash "frameset" "frameset -- Organize frames" h)
    (puthash "frame"    "frame -- Sub window" h)

    (puthash "bdo"      "bdo -- Text direction" h)

    (puthash "body"     "body -- Document body" h)
    (puthash "a"        "a -- Link" h)
    (puthash "p"        "p -- Paragraph" h)
    (puthash "span"     "span -- Group inline elements" h)
    (puthash "br"       "br -- Line break" h)
    (puthash "hr"       "hr -- Horizontal rule" h)
    (puthash "div"      "div -- Division/section" h)
    (puthash "img"      "img -- Image" h)
    (puthash "h1"       "h1 -- Header 1" h)
    (puthash "del"      "del -- Deleted text" h)
    (puthash "strike"   "strike -- (DEPRECEATED)" h)
    (puthash "u"        "u -- (DEPRECEATED)" h)
    (puthash "s"        "s -- (DEPRECEATED)" h)
    (puthash "ins"      "ins -- Inserted text" h)
    (puthash "sup"      "sup -- Superscript text" h)
    (puthash "center"   "center -- (DEPRECEATED)" h)
    (puthash "dir"      "dir -- (DEPRECEATED)" h)

    (puthash "blockquote" "blockquote -- Long quotation" h)
    (puthash "q"          "q -- Short quotation" h)
    (puthash "pre"      "pre -- Preformatted text" h)
    (puthash "applet"   "applet -- (DEPRECEATED)" h)
    (puthash "basefont" "basefont -- (DEPRECEATED)" h)
    (puthash "font"     "font -- (DEPRECEATED)" h)

    ;; The following elements are all font style elements. They are
    ;; not deprecated, but it is possible to achieve richer effects
    ;; using style sheets.
    (puthash "tt"       "tt -- Renders as teletype or mono spaced text" h)
    (puthash "i"        "i -- Renders as italic text" h)
    (puthash "b"        "b -- Renders as bold text" h)
    (puthash "big"      "big -- Renders as bigger text" h)
    (puthash "small"    "small -- Renders as smaller text" h)


    ;; The following tags are not deprecated, but it is possible to
    ;; achieve a much richer effect using style sheets:
    (puthash "em"       "em -- Renders as emphasized text" h)
    (puthash "strong"   "strong -- Renders as strong emphasized text" h)
    (puthash "dfn"      "dfn -- Defines a definition term" h)
    (puthash "code"     "code -- Defines computer code text" h)
    (puthash "samp"     "samp -- Defines sample computer code" h)
    (puthash "kbd"      "kbd -- Defines keyboard text" h)
    (puthash "var"      "var -- Defines a variable" h)
    (puthash "cite"     "cite -- Defines a citation" h)

    (puthash "ul"       "ul -- Unordered list" h)
    (puthash "ol"       "ol -- Ordered list" h)
    (puthash "li"       "li -- List element" h)
    (puthash "dl"       "dl -- Definition list" h)
    (puthash "dt"       "dt -- Definition term" h)
    (puthash "dd"       "dd -- Definition description" h)


    (puthash "fieldset" "fieldset -- Draw box around" h)
    (puthash "form"     "form -- User input form" h)
    (puthash "input"    "input -- Input field/checkbox etc" h)
    (puthash "textarea" "textarea -- Input multiline field" h)
    (puthash "button"   "button -- Push button" h)
    (puthash "label"    "label -- Label for control" h)
    (puthash "map"      "map -- Client side image map" h)
    (puthash "select"   "select -- Drop down list" h)
    (puthash "option"   "option -- Option in drop down list" h)
    (puthash "menu"     "menu -- (DEPRECEATED)" h)

    (puthash "object"   "object -- Embedded object" h)
    (puthash "param"    "param -- Object settings" h)

    (puthash "abbr"     "abbr -- Abbreviation" h)
    (puthash "address"  "address -- For addresses etc" h)
    (puthash "acronym"  "acronym -- May be used for lookup etc" h)

    (puthash "table"    "table -- Table" h)
    (puthash "caption"  "caption -- Table caption" h)
    (puthash "col"      "col -- Table column attributes" h)
    (puthash "colgroup"  "colgroup -- Table column group" h)
    (puthash "thead"    "thead -- Table header" h)
    (puthash "tbody"    "tbody -- Table body" h)
    (puthash "tfoot"    "tfoot -- Table footer" h)
    (puthash "tr"       "tr -- Table row" h)
    (puthash "td"       "td -- Table cell" h)

    h))

(defun nxhtml-short-tag-help (tag)
  "Display description of tag TAG.  If TAG is omitted, try tag at point."
  (interactive
   (let ((tag (xhtml-help-tag-at-point)))
     (unless (stringp tag)
       (setq tag (read-string "No tag at point. Give tag name: ")))
     (list tag)))
  (setq tag (downcase tag))
  (let ((desc (gethash tag nxhtml-completion-hash))
        (use-dialog-box nil))
    (unless desc
      (setq desc (concat tag " -- No short description available")))
    (when (y-or-n-p (concat desc ". Fetch more information from the Internet? "))
      (xhtml-help-browse-tag tag))))

(defcustom nxhtml-complete-with-help t
  "Complete with additional information.
If this is non-nil the completion window shows additional
information to each alternative.  This is currently only
implemented for tag names."
  :group 'nxhtml)

(defvar nxhtml-completing-with-help nil)

;; (defun nxhtml-rng-complete ()
;;   "Complete the string before point using the current schema.
;; Return non-nil if in a context it understands.

;; For certain attribute names prompt the user in a usefule way.
;; Currently this applies to the 'src' attribute."
;;   (interactive)
;;   (and rng-validate-mode
;;        (let ((lt-pos (save-excursion (search-backward "<" nil t)))
;; 	     xmltok-dtd)
;; 	 (and lt-pos
;; 	      (= (rng-set-state-after lt-pos) lt-pos)
;; 	      (or (rng-complete-tag lt-pos)
;; 		  (rng-complete-end-tag lt-pos)
;; 		  (rng-complete-attribute-name lt-pos)
;; 		  (nxhtml-rng-complete-attribute-value lt-pos))))))

;;(defun nxhtml-rng-complete-attribute-value (lt-pos)
;; (defun rng-complete-attribute-value (lt-pos)
;;   (when (save-excursion
;; 	  (re-search-backward rng-in-attribute-value-regex lt-pos t))
;;     (let ((name-start (match-beginning 1))
;; 	  (name-end (match-end 1))
;; 	  (colon (match-beginning 2))
;; 	  (value-start (1+ (match-beginning 3))))
;;       (message "name-start to end: %s" (buffer-substring-no-properties name-start name-end))(sit-for 2)
;;       (message "value-start to end: %s" (buffer-substring-no-properties value-start (match-end 3)))(sit-for 4)
;;       (and (rng-adjust-state-for-attribute lt-pos
;; 					   name-start)
;; 	   (if (string= (buffer-substring-no-properties name-start
;; 							(or colon name-end))
;; 			"xmlns")
;; 	       (rng-complete-before-point
;; 		value-start
;; 		(rng-strings-to-completion-alist
;; 		 (rng-possible-namespace-uris
;; 		  (and colon
;; 		       (buffer-substring-no-properties (1+ colon) name-end))))
;; 		"Namespace URI: "
;; 		nil
;; 		'rng-namespace-uri-history)
;; 	     (rng-adjust-state-for-attribute-value name-start
;; 						   colon
;; 						   name-end)
;; 	     (rng-complete-before-point
;; 	      value-start
;; 	      (rng-strings-to-completion-alist
;; 	       (rng-match-possible-value-strings))
;; 	      "Value: "
;; 	      nil
;; 	      'rng-attribute-value-history))
;; 	   (insert (char-before value-start))))
;;     t))

;; (defun nxhtml-complete-attribute-value-helper(attrib value)
;;   )

(defvar nxhtml-no-single-tags nil)
(defvar nxhtml-no-end-tags nil)

(defun nxhtml-rng-generate-qname-list (&optional string)
  (let ((ql (rng-generate-qname-list string)))
    (if nxhtml-complete-with-help
        (progn
          (when (or nxhtml-no-single-tags
                    nxhtml-no-end-tags)
            (let (ql2)
              (mapc (lambda(elt)
                      (unless (or (and nxhtml-no-end-tags
                                       (= ?/ (string-to-char elt)))
                                  (and nxhtml-no-single-tags
                                       (gethash elt nxhtml-single-tags-hash)))
                        (setq ql2 (cons elt ql2))))
                    ql)
              (setq ql ql2)))
          (mapcar (lambda(elt)
                    (let ((elt-h (gethash elt nxhtml-completion-hash)))
                      (if elt-h elt-h elt)))
                  ql))
      ql)))

(defun nxhtml-insert-completion (completion)
  (if nxhtml-complete-with-help
      (let ((completion (replace-regexp-in-string " -- .*" "" completion)))
        (if (featurep 'xhtmlin)
            (let ((len (- 0 (1+ (length completion)))))
              (insert completion)
              ;;(message "completion=%s" completion)(sit-for 2)
              (cond ((equal completion "a")
                     (delete-char len) ;; Delete <-char
                     (call-interactively 'xhtmlin-insert-a))
                    ((equal completion "img")
                     (delete-char len) ;; Delete <-char
                     (call-interactively 'xhtmlin-insert-img))
                    ((equal completion "link")
                     (when (y-or-n-p "Insert style sheet link? ")
                       (delete-char len) ;; Delete <-char
                       (call-interactively 'xhtmlin-insert-css)))
                    ((equal completion "script")
                     (when (y-or-n-p "Insert javascript link? ")
                       (delete-char len) ;; Delete <-char
                       (call-interactively 'xhtmlin-insert-javascript)))
                    ))
          (insert completion)))
    (insert completion)))

;;(defun rng-complete-qname-function (string predicate flag)
(defadvice rng-complete-qname-function (around nxhtml-rng-complete-qname-function-ad
                                               (string predicate flag)
                                               activate)
  ;;(if (not (eq major-mode 'nxhtml-mode))
  (if (not nxhtml-completing-with-help)
      ad-do-it
    (setq ad-return-value
          (let ((alist (mapcar (lambda (name) (cons name nil))
                               (nxhtml-rng-generate-qname-list string))))
            (cond ((not flag)
                   (try-completion string alist predicate))
                  ((eq flag t)
                   (all-completions string alist predicate))
                  ((eq flag 'lambda)
                   (and (assoc string alist) t)))))))

(defadvice rng-complete-before-point (around nxhtml-rng-complete-before-point-ad
                                             (start table prompt &optional predicate hist)
                                             activate)
  "Complete text between START and point.
Replaces the text between START and point with a string chosen using a
completion table and, when needed, input read from the user with the
minibuffer.
Returns the new string if either a complete and unique completion was
determined automatically or input was read from the user. Otherwise,
returns nil.
TABLE is an alist, a symbol bound to a function or an obarray as with
the function `completing-read'.
PROMPT is the string to prompt with if user input is needed.
PREDICATE is nil or a function as with `completing-read'.
HIST, if non-nil, specifies a history list as with `completing-read'."
  (if (not (eq major-mode 'nxhtml-mode))
      ad-do-it
    (setq nxhtml-completing-with-help nxhtml-complete-with-help)
    (setq ad-return-value
          (let* ((orig (buffer-substring-no-properties start (point)))
                 (completion (try-completion orig table predicate)))
            (cond ((not completion)
                   (if (string= orig "")
                       (message "No completions available")
                     (message "No completion for %s" (rng-quote-string orig)))
                   (ding)
                   nil)
                  ((eq completion t) orig)
                  ((not (string= completion orig))
                   (delete-region start (point))
                   (nxhtml-insert-completion completion)
                   (cond ((not (rng-completion-exact-p completion table predicate))
                          (message "Incomplete")
                          nil)
                         ((eq (try-completion completion table predicate) t)
                          completion)
                         (t
                          (message "Complete but not unique")
                          nil)))
                  (t
                   (setq completion
                         (let ((saved-minibuffer-setup-hook
                                (default-value 'minibuffer-setup-hook)))
                           (add-hook 'minibuffer-setup-hook
                                     'minibuffer-completion-help
                                     t)
                           (unwind-protect
                               (completing-read prompt
                                                table
                                                predicate
                                                nil
                                                orig
                                                hist)
                             (setq-default minibuffer-setup-hook
                                           saved-minibuffer-setup-hook))))
                   (delete-region start (point))
                   (nxhtml-insert-completion completion)
                   completion))))))



;;(setq xhtmlin-insertable 'nxhtml-insertable)

;;                 (lambda(name)
;;                   (if (file-directory-p name)
;;                       t
;;                     (let ((ext (file-name-extension name)))
;;                       (when ext
;;                         (eq t
;;                             (compare-strings
;;                              "css" nil nil
;;                              ext nil nil
;;                              t))))))
;; (defun nxhtml-check-insertable(tag)
;;   (let ((len (nxhtml-tag-insertable tag)))
;;     (unless len
;;       (when (y-or-n-p
;;              (concat
;;               "Inserting a '" tag
;;               "' here is incorrect. Do it anyway? "))
;;         (setq len 0)))
;;     len))

;; (defun nxhtml-insert-css()
;;   "Insert an xhtml header css link tag.
;; URL can be a http url or a file. It can be checked if it exists,
;; see ´xhtmlin-get-url'."
;;   (interactive)
;;   (let ((len 0))
;;     (when (fboundp 'nxhtml-check-insertable)
;;       (setq len (nxhtml-check-insertable "link")))
;;     (when len
;;       (let ((url (expand-file-name
;;                   (read-file-name "CSS file: " ))))
;;         (insert (substring (xhtmlin-css url) len))))))

;;(defun nxhtml-insert-javascript))
;;(defun nxhtml-insert-img))
;;(defun nxhtml-insert-a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional completion

(defadvice rng-complete-attribute-value (around nxhtml-rng-complete-attribute-value-ad
                                                (lt-pos)
                                                activate)
  (if (not (eq major-mode 'nxhtml-mode))
      ad-do-it
    (let* (val
           init
           delimiter
          (in-attr-val
           (save-excursion
             (re-search-backward rng-in-attribute-value-regex lt-pos t)))
          (in-xml-attr-val
           (unless in-attr-val
             (save-excursion
               (re-search-backward nxhtml-in-xml-attribute-value-regex lt-pos t))))
          )
      (when (or in-attr-val in-xml-attr-val)
        (save-match-data
          (re-search-forward "\\=[^<> \t\r\n\"]*" nil t))
        (let* ((name-start (match-beginning 1))
               (name-end (match-end 1))
               (colon (match-beginning 2))
               (attr (buffer-substring-no-properties name-start
                                                     (or colon name-end)))
               (value-start (1+ (match-beginning 3)))
               (tag (save-excursion
                      (when (search-backward-regexp "<[[:alpha:]]+" nil t)
                        (match-string 0)))))
          (setq init (buffer-substring-no-properties value-start (point)))
          (setq delimiter (char-before value-start))
          (if in-xml-attr-val
              (cond ((string= "encoding" attr)
                     ;; Give a default that works in browsers today
                     (setq val (nxhtml-coding-systems-complete
                                init
                                (symbol-name nxhtml-default-encoding))))
                    ((string= "version" attr)
                     (setq val "1.0")))
            (cond ((string= "href" attr)
                   (cond ((string= "<a" tag)
                          (setq val (nxhtml-read-url t init)))
                         ((string= "<base" tag)
                          (setq val (nxhtml-read-url nil init nil "Base")))
                         ((string= "<area" tag)
                          (setq val (nxhtml-read-url nil init)))
                         ((string= "<link" tag)
                          (setq val (nxhtml-read-url nil init)))
                         (t
                          (setq val (nxhtml-read-url nil init)))))
                  ((string= "src" attr)
                   (cond ((string= "<img" tag)
                          (setq val (nxhtml-read-url nil init 'nxhtml-image-url-predicate "Image")))
                         ((string= "<script" tag)
                          (setq val (nxhtml-read-url nil init 'nxhtml-script-url-predicate "Script")))
                         ((string= "<input" tag)
                          (setq val (nxhtml-read-url nil init 'nxhtml-image-url-predicate "Image")))
                         ((string= "<frame" tag)
                          (setq val (nxhtml-read-url nil init nil "Frame Source")))
                         ((string= "<iframe" tag)
                          (setq val (nxhtml-read-url nil init nil "Frame Source")))
                         (t
                          (setq val (nxhtml-read-url nil init)))))))))
      (if (not val)
          ad-do-it
        (when init
          (delete-char (- (length init))))
        (insert val)
        (unless (eq (char-after) delimiter)
          (insert delimiter))
        (setq ad-return-value t)))))

(defun nxhtml-read-url(&optional allowed-types initial-contents extra-predicate prompt-prefix)
  (let* ((url-type (nxhtml-read-url-type allowed-types initial-contents))
         (base-prompt (cond ((eq url-type 'local-file-url)
                             "File: ")
                            ((eq url-type 'web-url)
                             "Web URL: ")
                            ((eq url-type 'mail-url)
                             "e-Mail address: ")
                            ((eq url-type 'any-url)
                             "Any URL-type: ")
                            (t
                             (error "Internal error: bad url-type=%s" url-type))))
         prompt
         type-predicate
         url
         (bad-url initial-contents)
         (default-directory (if buffer-file-name
                                (file-name-directory buffer-file-name)
                              default-directory)))
    (when prompt-prefix
      (setq base-prompt (concat prompt-prefix " " base-prompt)))
    (setq nxhtml-predicate-error "")
    (cond ((eq url-type 'local-file-url)
           )
          ((eq url-type 'web-url)
           )
          ((eq url-type 'mail-url)
           (setq type-predicate 'nxhtml-mailto-predicate)
           (when (and (stringp bad-url)
                      (<= 7 (length bad-url))
                      (string= "mailto:" (substring bad-url 0 7)))
             (setq bad-url (substring bad-url 7)))
           )
          )
    (while (not url)
      (setq prompt (concat nxhtml-predicate-error " " base-prompt))
      (cond ((eq url-type 'local-file-url)
             (setq url (read-file-name prompt nil "" nil bad-url extra-predicate))
             (when (< 0 (length url))
               (setq url (file-relative-name
                          (expand-file-name url)))))
            ((eq url-type 'web-url)
             (setq url (read-from-minibuffer prompt bad-url nil nil
                                             'nxhtml-read-web-url-history
                                             t)))
            ((eq url-type 'mail-url)
             (setq url (read-from-minibuffer prompt bad-url nil nil
                                             'nxhtml-read-mail-url-history
                                             t)))
            (t
             (setq url (read-from-minibuffer prompt bad-url nil nil
                                             'nxhtml-read-url-history
                                             t))))
      (when (or (and type-predicate
                     (not (funcall type-predicate url)))
                (and extra-predicate
                     (not (funcall extra-predicate url))))
        (setq bad-url url)
        (setq url)))
    (when (eq url-type 'mail-url)
      (setq url (concat "mailto:" url)))
    url))

(defun nxhtml-read-url-type-help()
  (interactive)
  (let ((name "*URL Type Help*"))
    (with-output-to-temp-buffer name
      (princ "Help for URL type choice:\n\n")
      (princ "Type just one letter to choose URL type.  ")
      (princ "This will affect prompting and in some cases do basic checks.  ")
      (princ "\n\n")
      (princ "Use C-g to quit.")
      (with-current-buffer name
        (fill-region (point-min) (point-max))))))

(defun nxhtml-read-url-type(allowed url-beginning)
  (let ((prompt "URL-type (")
        (map (make-sparse-keymap))
        (beg-type (elt (url-generic-parse-url url-beginning) 0))
        choice)
    (define-key map "?" 'nxhtml-read-url-type-help)
    (define-key map [(control ?g)] 'abort-recursive-edit)
    (define-key map [t] 'ignore)
    (cond ((string= "mailto" beg-type)
           (setq allowed '(?m)))
          ((or (string= "http"  beg-type)
               (string= "https" beg-type)
               (string= "ftp"   beg-type))
           (setq allowed '(?w)))
          ((= 1 (length beg-type)) ;; w32
           (setq allowed '(?f)))
          )
    (if allowed
        (when (eq allowed t)
          (setq allowed '(?f ?w ?m)))
      (setq allowed '(?f ?w)))
    (if (= 1 (length allowed))
        (setq choice (car allowed))
      (when (memq ?f allowed)
        (define-key map "f" 'exit-minibuffer)
        (setq prompt (concat prompt "f-local file, ")))
      (when (memq ?m allowed)
        (define-key map "m" 'exit-minibuffer)
        (setq prompt (concat prompt "m-mail, ")))
      (when (memq ?w allowed)
        (define-key map "w" 'exit-minibuffer)
        (setq prompt (concat prompt "w-web url, ")))
      (setq prompt (concat prompt "? for help): "))
      (read-from-minibuffer prompt nil map)
      (setq choice last-input-char))
  (cond ((memq choice '(?f ?F))
         'local-file-url)
        ((memq choice '(?w ?W))
         'web-url)
        ((memq choice '(?m ?M))
         'mail-url)
        )))

(defvar nxhtml-read-url-history nil)
(defvar nxhtml-read-web-url-history nil)
(defvar nxhtml-read-mail-url-history nil)

(defconst nxhtml-in-xml-attribute-value-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   ;;"<w\\(?::w\\)?\
   "<\\?xml\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
\[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
\[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\(\"[^\"]*\\|'[^']*\\)\\="
   t
   t))

(defvar nxhtml-predicate-error nil)

(defun nxhtml-mailto-predicate(url)
  "Tries to match a mailto url.
This is not supposed to be entirely correct."
  (setq nxhtml-predicate-error nil)
  ;; Local pattern copied from gnus.
  (let ((r (concat "^"
                   ;;"mailto:"
                   "[a-z0-9$%(*-=?[_][^<>\")!;:,{}]*"
                   "\@"
                   "\\(?:[a-z0-9\-]+\.\\)+[a-z0-9]\\{2,4\\}$"))
        (case-fold-search t))
    ;;(message "mailpred") (sit-for 1)
    (if (string-match r url)
        t
      (setq nxhtml-predicate-error "Malformed email address.")
      nil)))

(defcustom nxhtml-image-completion-pattern
  "\\.\\(?:png\\|jpg\\|jpeg\\|gif\\)$"
  "Pattern for matching image URLs in completion."
  :type 'regexp)

(defun nxhtml-image-url-predicate(url)
  (setq nxhtml-predicate-error nil)
  (if (or (file-directory-p url)
          (string-match nxhtml-image-completion-pattern url))
      t
    (setq nxhtml-predicate-error "Does not match image file name pattern.")
    nil
    ))

(defcustom nxhtml-script-completion-pattern
  "\\.\\(?:js\\)$"
  "Pattern for matching src URLs in completion in script tags."
  :type 'regexp)

(defun nxhtml-script-url-predicate(url)
  (setq nxhtml-predicate-error nil)
  (if (or (file-directory-p url)
          (string-match nxhtml-script-completion-pattern url))
      t
    (setq nxhtml-predicate-error "Does not match script file name pattern.")
    nil
    ))

(defun nxhtml-coding-systems-complete(init default)
  (let (coding-systems
        hist-num
        (n 0)
        hist)
    (unless (and init (< 0 (length init)))
      (setq init default))
    (mapc (lambda(coding-system)
            (let ((mime-charset (coding-system-get coding-system 'mime-charset)))
              (when mime-charset
                (setq coding-systems (cons
                                      (symbol-name mime-charset)
                                      coding-systems)))))
          (coding-system-list t))
    (setq coding-systems (sort coding-systems 'string=))
    (mapc (lambda(coding-system)
            (unless (< 0 (length coding-system))
              (error "len=0"))
            (setq n (1+ n))
            (when (string= coding-system init) (setq hist-num n)))
          coding-systems)
    (if hist-num
        (setq hist (cons 'coding-systems hist-num))
      (setq hist 'coding-systems))
    (completing-read "Encoding (coding system): "
                     coding-systems nil t init hist)))

(defun nxhtml-complete-and-insert()
  "Perform XHTML completion at point.
This is merely an extended version of `nxml-complete' with the following changes:

- May propose to insert a tag even if no preceeding '<'
  character.
- If region is visible and active then it is tried if a tag can
  be surrounded.
- If in attribute value then in some cases relevant prompting is
  performed.
- Completes xml version and encoding.
- Completes an empty file, ie insert a skeleton.

Note: This function does not currently use the state provided by
the nxml and rng functions directly.  Instead it searches the
environment near point to decide what to do.
"
  (interactive)
  (let (res
        (where (nxhtml-check-where)))
    (or (when (= 0 (buffer-size))
          (nxhtml-empty-page-completion))
        (when (and mark-active
                   transient-mark-mode
                   (eq where 'in-text))
          (nxhtml-insert-tag))
        (progn
          (cond ((memq where '(in-start-tag in-closed-start-tag in-end-tag))
                 (re-search-forward "\\=/?[a-z]*" nil t))
                ((memq where '(in-attr))
                 (re-search-forward "\\=[a-z]*=" nil t))
                ((memq where '(in-attr-val in-xml-attr-val))
                 (re-search-forward "\\=[^<>\" \t\r\n]*" nil t))
                )
          (when (run-hook-with-args-until-success 'nxml-completion-hook)
            (when (re-search-backward "[^=]\"\\=" nil t)
              (forward-char) (delete-char 1)
              ;;(undo-start) (undo-more 1)
              )
            t))
        (when (and (not where)
                   (char-before)
                   (= ?\" (char-before)))
          nil)
        (when (or (when (char-before) (= ?> (char-before)))
                  (eq where 'in-text))
          (setq res t)
          (nxhtml-insert-tag))
        ;; Eventually we will complete on entity names here.
        res
        (progn
          (ding)
          (message "Cannot complete in this context")))))

(defvar nxhtml-in-proc-instr-back-regex "<\\?[^<>]*\\=")
(defvar nxhtml-in-proc-instr-forw-regex "\\=[^<>]*\\?>")

(defun nxhtml-check-where()
  "Get a state for `nxhtml-complete-and-insert'."
  (let ((p (point))
        res)
    (cond ((looking-back "<!--[^<>]*\\=" 1 t)
           (setq res 'in-comment))
          ((progn
            (setq face (get-char-property (point) 'face))
            (when (memq face '(nxml-comment-content-face
                               nxml-comment-delimiter-face))
              (setq res 'in-comment)))
           t)
          ((looking-back nxhtml-in-proc-instr-back-regex 1 t)
           (setq res 'in-proc-instr))
          ((looking-back "<!D[^>]*\\=" 1 t)
           (setq res 'in-doctype))
          ((looking-back ">[^<]*" 1 t)
           (setq res 'in-text))
          ((looking-back rng-in-start-tag-name-regex 1 t)
           (setq res 'in-tag-start)
           (when (looking-at "\\=[^<]*>")
             (setq res 'in-closed-start-tag)))
          ((looking-back rng-in-end-tag-name-regex 1 t)
           (setq res 'in-tag-end))
          ((looking-back rng-in-attribute-regex 1 t)
           (setq res 'in-attr))
          ((looking-back rng-in-attribute-value-regex 1 t)
           (setq res 'in-attr-val))
          ((looking-back "\"")
           (setq res 'after-attr-val))
          )
    ;;(message "res=%s" res)(sit-for 1)
    (unless res
      (error "Could not find a state for completion"))
    res))

(defun nxhtml-insert-tag()
  (interactive)
  (let* ((in-attr (save-excursion (re-search-backward rng-in-attribute-regex 1 t)))
         (in-tag-start (save-excursion (re-search-backward rng-in-start-tag-name-regex 1 t)))
         (in-tag-end (save-excursion (re-search-backward rng-in-end-tag-name-regex 1 t)))
         (in-attr-val (save-excursion (re-search-backward rng-in-attribute-value-regex 1 t)))
         (start-closed (when in-tag-start
                         (save-excursion
                           (re-search-forward "\\=[^<]*>" nil t))))
         (point     (copy-marker (point)))
         (beginning (copy-marker (if mark-active (region-beginning) (point)) t))
         (end       (copy-marker (if mark-active (region-end)       (point)) t))
         complete-overlay
         res
         (lt-pos (or in-attr in-tag-start))
         (mark-was-active mark-active)
         tag-name
         (num-undo 0)
         )
    (when (or in-attr-val
              in-tag-end)
      (error "Can't insert tag here"))
    (when (and in-tag-start
               start-closed)
      (error "Can't insert tag inside start tag"))
    (setq complete-overlay (make-overlay beginning end))
    (overlay-put complete-overlay 'face 'region)
    (goto-char beginning)
    (if (not (or in-tag-start
                 in-attr))
        (progn
          (setq lt-pos (point))
          (insert "< ")
          (setq num-undo (1+ num-undo))
          (backward-char))
      (goto-char lt-pos)
      (re-search-forward "\\=<[[:alpha:]]*"))
    (while (rng-do-some-validation) nil)
    (rng-validate-done)
    (let ((inhibit-quit t)
          (nxhtml-no-single-tags mark-was-active)
          (nxhtml-no-end-tags t)
          (pos-before (point))
          )
      (unwind-protect
          (progn
            (setq res (rng-complete))
            (unless (= pos-before (point))
              (setq num-undo (1+ num-undo))))
        (delete-overlay complete-overlay)
        (unless res
          ;;(message "num-undo=%s" num-undo)(sit-for 4)
          (undo-start)
          (undo-more num-undo))))
    (when res
      (setq tag-name (buffer-substring-no-properties (1+ lt-pos) (point)))
      (if (= 0 (length tag-name))
          (setq res nil)
        (unless (gethash tag-name nxhtml-completion-hash)
          (setq res (y-or-n-p "Unknown tag, do you still want to insert it? "))
          )))
    (if res
        (let ((p (1+ (point))))
          (goto-char beginning)
          (if (gethash tag-name nxhtml-single-tags-hash)
              (insert "/>")
            (insert ">")
            (while (rng-do-some-validation) nil)
            (rng-validate-done)
            (let ((wants-end-tag t))
              (goto-char lt-pos)
              (condition-case err
                  (save-excursion
                    (nxml-forward-element)
                    (unless (y-or-n-p "There is already a matching end tag. Still add another? ")
                      (setq wants-end-tag nil)))
                (error nil))
              (when wants-end-tag
                (goto-char end)
                (nxml-finish-element))))
          (goto-char p)
          t)
      (undo-start)
      (undo-more num-undo)
      (set-mark (if (= point end)
                    beginning
                  (assert (= point beginning))
                  end))
      (goto-char point)
      (run-with-idle-timer 0 nil (lambda() (setq mark-active t)))
      nil)))

;; (defun testf()
;;   (let (res)
;;     (condition-case err
;;         (progn
;;           (save-excursion
;;             (nxml-forward-element))
;;           (setq res t))
;;       (error nil))
;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML migration
(defun nxhtml-downcase-tags-and-attributes()
  (interactive)
  (unless (memq major-mode '(nxhtml-mode nxml-mode))
    (error "Can only be used in nxhtml-mode or nxml-mode"))
  (nxml-fontify-buffer)
  (let ((start-tag-regexp 
         "<\\(?:[_[:alpha:]][-._[:alnum:]]*\\)\\(?::\\(?:[_[:alpha:]][-._[:alnum:]]*\\)?\\)?")
        (end-tag-regexp
         "</\\(?:[_[:alpha:]][-._[:alnum:]]*\\)\\(?::\\(?:[_[:alpha:]][-._[:alnum:]]*\\)?\\)?")
        (attr-regexp
         "[[:space:]]+\\(\\(?:[_[:alpha:]][-._[:alnum:]]*\\)\\(?::\\(?:[_[:alpha:]][-._[:alnum:]]*\\)\\)?\\)[[:space:]]*=")
        )
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp start-tag-regexp nil t)
        (when (eq (get-text-property (1+ (match-beginning 0)) 'face)
                  'nxml-element-local-name-face)
          (downcase-region (match-beginning 0) (match-end 0))))
      (goto-char (point-min))
      (while (search-forward-regexp end-tag-regexp nil t)
        (when (eq (get-text-property (+ 2 (match-beginning 0)) 'face)
                  'nxml-element-local-name-face)
          (downcase-region (match-beginning 0) (match-end 0))))
      (goto-char (point-min))
      (while (search-forward-regexp attr-regexp nil t)
        (when (eq (get-text-property (match-beginning 1) 'face)
                  'nxml-attribute-local-name-face)
          (downcase-region (match-beginning 1) (match-end 1)))))))

(defun nxhtml-add-attr-dquotes()
  (interactive)
  (unless (memq major-mode '(nxhtml-mode nxml-mode))
    (error "Can only be used in nxhtml-mode or nxml-mode"))
  (nxml-fontify-buffer)
  (let ((attr-regexp
         "[[:space:]]+\\(\\(?:[_[:alpha:]][-._[:alnum:]]*\\)\\(?::\\(?:[_[:alpha:]][-._[:alnum:]]*\\)\\)?\\)[[:space:]]*=\\(.*?\\)[[:space:]]")
        )
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp attr-regexp nil t)
        (when (eq (get-text-property (match-beginning 1) 'face)
                  'nxml-attribute-local-name-face)
          (unless (memq (char-after (match-beginning 2)) '(?\" ?'))
            (goto-char (match-end 2))
            (insert-char ?\" 1)
            (goto-char (match-beginning 2))
            (insert-char ?\" 1)))))))

(when (featurep 'tidy)
  (tidy-build-menu nxhtml-mode-menu-map))
;; (tidy-build-menu nxhtml-mode-map)
;; (easy-menu-add tidy-menu nxhtml-mode-map)
;; (easy-menu-add tidy-menu nxhtml-mode-menu-map)


(defun my-nxhtml-mode-hook () "Customize my html-mode."
  (tidy-build-menu nxhtml-mode-map)
  (tidy-build-menu nxhtml-mode-menu-map)
  ;;(local-set-key [(control c) (control c)] 'tidy-buffer)
  ;;(setq sgml-validate-command "tidy")
  )

(add-hook 'nxhtml-mode-hook 'my-nxhtml-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'nxhtml)

;;; nxhtml.el ends here
