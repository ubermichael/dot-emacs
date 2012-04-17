;;; html-toc.el --- Building and updating TOC for a site

;; Copyright (C) 2005, 2006 Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Wed Feb 01 14:40:13 2006
(defconst html-toc:version "0.4");; Version:
;; Last-Updated: Sat Apr 29 23:14:59 2006 (7200 +0200)
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl', `fupd', `html-site'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Create table of contents for a static web site.  See
;;  `html-toc-write-toc-file' and `html-toc-write-frames-file' for
;;  more info.
;;
;;  To use this you can add (require 'html-toc) to your .emacs.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'fupd)
;;(require 'html-move)
(require 'html-site)
;;(require 'dom)

(defconst html-toc-mark-begin  "<!-- html-toc START -->")
(defconst html-toc-mark-middle "<!-- html-toc MIDDLE -->")
(defconst html-toc-mark-end    "<!-- html-toc END -->")

(defvar html-toc-header
    "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <base href=\"\" target=\"html-toc-Document\" />
    <link href=\"%%HTML-TOC-CSS%%\" rel=\"StyleSheet\" />
    <title>Table of contents</title>
    <script type=\"text/javascript\">
      function html_toc_hs(loc) {
          var subnode = loc.nextSibling;
          if (subnode) {
              subnode = subnode.nextSibling;
          } else {
              subnode = loc.parentNode.nextSibling.nextSibling;
          }
          var chstyle = subnode.style;
          chstyle.display = chstyle.display==''?'none':'';
      }
    </script>
  </head>
  <body>
")
;;       function html_toc_hs(loc) {
;;         var chstyle = loc.nextSibling.nextSibling.style;
;;         chstyle.display = chstyle.display==''?'none':'';
;;       }

(defvar html-toc-footer
  "
  </body>
</html>\n")

(defvar html-toc-css
  ".html-toc-hs:hover { background-color:#b4eeb4; }
.html-toc-hs { cursor:pointer; }")

;; (defcustom html-toc-file-default-name "html-toc.html"
;;   "Default file name sans directory for TOC file.
;; This is relative to `html-move-site-directory'."
;;   :type 'string)

;; (defun html-toc-get-site()
;;   (unless html-move-site-directory
;;     (call-interactively 'html-move-set-site-directory))
;;   (unless html-move-site-directory
;;     (error "You must set local file web site directory first"))
;;   html-move-site-directory)

;; (defun html-toc-file()
;;   (html-toc-get-site)
;;   (expand-file-name html-toc-file-default-name html-move-site-directory))

(defun html-toc-css-file()
  (concat (file-name-sans-extension (html-site-current-toc-file)) ".css"))

(defun html-toc-write-css-file()
  "Write TOC style sheet file.
Return the name of that file."
  (interactive)
  (let* ((css-file (html-toc-css-file))
         (css-name (file-name-nondirectory css-file))
         (css-dir  (file-name-directory css-file)))
    (setq css-file
          (expand-file-name
           (read-file-name "TOC style sheet file to write: "
                           css-dir
                           css-name
                           nil
                           css-name)
           css-dir))
    (let ((osbo (fupd-has-contents css-file html-toc-css)))
      (unless (fupd-ok osbo)
        (when (or (not (file-exists-p css-file))
                  (y-or-n-p (concat "TOC style sheet file ("
                                    css-file
                                    ") already exists. Update? ")))
          (fupd-update-file css-file html-toc-css)))
      ;;(message "before kill-new-buffer")(sit-for 4)
      (fupd-kill-new-buffer osbo))
    css-file))

(defun html-toc-write-toc-file()
  "Write a table of contents for a web site.
The purpose of this function is to quickly create a table of
contents.  You probably want to edit it or transform it to
another format.

First write a CSS style sheet file with the same name as the file
below, but with extension .css if it does not exist.  It will
contain the value of `html-toc-css'.

Build the table of content from the information in
`html-site-current-page-list'.  Write it to the file
`html-site-current-toc-file' and return that file name.

If that file exists update it with new information.  The updated
information should be edited manually afterwards.

To refresh the table of contents delete the file or just delete
everything between `html-toc-mark-begin' and `html-toc-mark-end'
in the table of contents file before running this function.

When viewed in a browser the table of contents can be
expanded/collapsed by clicking on subheaders (if JavaScript is
allowed).
"
  (interactive)
  (html-site-ensure-site-defined)
  (let* (css-file
         (toc-file (html-site-current-toc-file))
         (page-file (html-site-current-page-list))
         page-lines
         sub-files
         old-toc-found
         hrefs-old
         hrefs-added
         hrefs-obsolete)
    (unless (< 0 (length toc-file))
      (error "There is no name for the table of content file in site \"%s\""
             html-site-current))
    (unless (< 0 (length page-file))
      (error "There is no name for the pages file in site \"%s\""
             html-site-current))
    (setq css-file (html-toc-write-css-file))
    (when css-file
      (with-temp-buffer
        (insert-file-contents page-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring (point) (line-end-position)))
                 (line-parts (split-string line "\\s-+###\\s-+"))
                 (level (nth 0 line-parts))
                 (title (nth 1 line-parts))
                 (url   (nth 2 line-parts)))
            (setq sub-files (cons url sub-files))
            (setq page-lines (cons line-parts page-lines)))
          (forward-line)
          ))
            ;;(error "level=%s, title=%s, url=%s" level title url))))
      (setq page-lines (reverse page-lines))
      (with-current-buffer (find-file-noselect toc-file)
        ;;(erase-buffer)
        (when (= 0 (buffer-size))
          (insert (replace-regexp-in-string "%%HTML-TOC-CSS%%"
                                            (file-relative-name
                                             css-file
                                             (file-name-directory toc-file))
                                            html-toc-header)
                  "\n\n"
                  html-toc-mark-begin
                  "\n"
                  html-toc-mark-middle
                  "\n"
                  html-toc-mark-end
                  "\n\n"
                  html-toc-footer))
        (goto-char (point-min))
        (let ((old-toc-end (search-forward html-toc-mark-end nil t))
              old-toc-begin
              old-toc-middle)
          (goto-char (point-max))
          (unless old-toc-end (error "Can't find %s" html-toc-mark-end))
          (setq old-toc-middle (search-backward html-toc-mark-middle nil t))
          (unless old-toc-middle (error "Can't find %s" html-toc-mark-middle))
          (setq old-toc-begin  (search-backward html-toc-mark-begin  nil t))
          (unless old-toc-begin (error "Can't find %s" html-toc-mark-begin))
          ;;(message "b=%s, m=%s, e=%s" old-toc-begin old-toc-middle old-toc-end)(sit-for 4)
          (setq hrefs-old
                (html-toc-get-hrefs
                 (html-toc-parse-toc
                  (buffer-substring-no-properties
                   old-toc-begin old-toc-middle))))
          (when hrefs-old
            (setq old-toc-found t)
            (dolist (line page-lines)
              (let ((sf (nth 2 line)))
                (unless (member sf hrefs-old)
                  (setq hrefs-added (cons line hrefs-added)))))
            (dolist (ho hrefs-old)
              (unless (member ho sub-files)
                (setq hrefs-obsolete (cons ho hrefs-obsolete)))))
          (let (toc-start)
            ;;(move-beginning-of-line) (unless (looking-at "\n") (insert "\n"))
            ;;(message "hrefs-old=%s" hrefs-old)(sit-for 2)
            (if hrefs-old
                (progn
                  (goto-char old-toc-middle)
                  (delete-region old-toc-middle old-toc-end)
                  (setq toc-start (point-marker))
                  (insert html-toc-mark-middle "\n")
                  (when hrefs-added
                    ;;(message "hrefs-added=%s" hrefs-added)(sit-for 2)
                    (insert "<b>New files:</b>\n")
                    (html-toc-insert-toc hrefs-added))
                  (when hrefs-obsolete
                    ;;(message "hrefs-obsolete=%s" hrefs-obsolete)(sit-for 2)
                    (insert "<b>Files removed:</b>\n")
                    (insert "<ul>\n")
                    (dolist (ho hrefs-obsolete)
                      (insert "<li>" ho "</li>\n"))
                    (insert "</ul>\n"))
                  (insert html-toc-mark-end)
                  )
              (delete-region old-toc-begin old-toc-end)
              (setq toc-start (point-marker))
              (insert html-toc-mark-begin "\n")
              (html-toc-insert-toc page-lines)
              (insert html-toc-mark-middle "\n")
              (insert html-toc-mark-end)
              )
            (unless (looking-at "\n") (insert "\n"))
            (indent-region toc-start (point-marker))
            ))
        (save-buffer))
      toc-file)))

;; (defun old-html-toc-write-toc-file()
;;   (interactive)
;;   (let* (css-file
;;          (toc-file (html-site-current-toc-file))
;;          (sub-files (html-site-get-sub-files html-move-site-directory
;;                                             html-toc-site-files-re))
;;          old-toc-found
;;          hrefs-old
;;          hrefs-added
;;          hrefs-obsolete)
;;     (setq sub-files
;;           (sort (mapcar (lambda(full-file)
;;                           (assert (file-exists-p full-file))
;;                           (file-relative-name full-file html-move-site-directory))
;;                         sub-files)
;;                 'string<))
;;     (setq sub-files (delete html-toc-file-default-name sub-files))
;;     ;;(message "sub-files=%s" sub-files)(sit-for 2)
;;     (setq css-file (html-toc-write-css-file))
;;     (when css-file
;;       (with-current-buffer (find-file-noselect toc-file)
;;         ;;(erase-buffer)
;;         (when (= 0 (buffer-size))
;;           (insert (replace-regexp-in-string "%%HTML-TOC-CSS%%"
;;                                             (file-relative-name
;;                                              css-file
;;                                              (file-name-directory toc-file))
;;                                             html-toc-header)
;;                   "\n\n"
;;                   html-toc-mark-begin
;;                   "\n"
;;                   html-toc-mark-middle
;;                   "\n"
;;                   html-toc-mark-end
;;                   "\n\n"
;;                   html-toc-footer))
;;         (goto-char (point-min))
;;         (let ((old-toc-end (search-forward html-toc-mark-end nil t))
;;               old-toc-begin
;;               old-toc-middle)
;;           (goto-char (point-max))
;;           (unless old-toc-end (error "Can't find %s" html-toc-mark-end))
;;           (setq old-toc-middle (search-backward html-toc-mark-middle nil t))
;;           (unless old-toc-middle (error "Can't find %s" html-toc-mark-middle))
;;           (setq old-toc-begin  (search-backward html-toc-mark-begin  nil t))
;;           (unless old-toc-begin (error "Can't find %s" html-toc-mark-begin))
;;           ;;(message "b=%s, m=%s, e=%s" old-toc-begin old-toc-middle old-toc-end)(sit-for 4)
;;           (setq hrefs-old
;;                 (html-toc-get-hrefs
;;                  (html-toc-parse-toc
;;                   (buffer-substring-no-properties
;;                    old-toc-begin old-toc-middle))))
;;           (when hrefs-old
;;             (setq old-toc-found t)
;;             (dolist (sf sub-files)
;;               (unless (member sf hrefs-old)
;;                 (setq hrefs-added (cons sf hrefs-added))))
;;             (dolist (ho hrefs-old)
;;               (unless (member ho sub-files)
;;                 (setq hrefs-obsolete (cons ho hrefs-obsolete)))))
;;           (let (toc-start)
;;             ;;(move-beginning-of-line) (unless (looking-at "\n") (insert "\n"))
;;             ;;(message "hrefs-old=%s" hrefs-old)(sit-for 2)
;;             (if hrefs-old
;;                 (progn
;;                   (goto-char old-toc-middle)
;;                   (delete-region old-toc-middle old-toc-end)
;;                   (setq toc-start (point-marker))
;;                   (insert html-toc-mark-middle "\n")
;;                   (when hrefs-added
;;                     ;;(message "hrefs-added=%s" hrefs-added)(sit-for 2)
;;                     (insert "<b>New files:</b>\n")
;;                     (html-toc-insert-toc hrefs-added))
;;                   (when hrefs-obsolete
;;                     ;;(message "hrefs-obsolete=%s" hrefs-obsolete)(sit-for 2)
;;                     (insert "<b>Files removed:</b>\n")
;;                     (insert "<ul>\n")
;;                     (dolist (ho hrefs-obsolete)
;;                       (insert "<li>" ho "</li>\n"))
;;                     (insert "</ul>\n"))
;;                   (insert html-toc-mark-end)
;;                   )
;;               (delete-region old-toc-begin old-toc-end)
;;               (setq toc-start (point-marker))
;;               (insert html-toc-mark-begin "\n")
;;               (html-toc-insert-toc sub-files)
;;               (insert html-toc-mark-middle "\n")
;;               (insert html-toc-mark-end)
;;               )
;;             (unless (looking-at "\n") (insert "\n"))
;;             (indent-region toc-start (point-marker))
;;             ))
;;         (save-buffer))
;;       toc-file)))

(defcustom html-toc-no-dir-line nil
  "No extra dir line in TOC written by `html-toc-write-toc-file'."
  :type 'boolean)

(defun html-toc-insert-toc(page-lines)
  (insert "<ul>\n")
  (let* ((curr-level 0)
         (this-level)
         (dir-title)
         (full-file)
         (file)
         (title)
         (pending-li nil)
         (site-directory (html-site-current-site-dir))
         (toc-rel-file (file-relative-name toc-file site-directory)))
    (dolist (line page-lines)
      (setq file (nth 2 line))
      (setq title (nth 1 line))
      (setq this-level (string-to-number (nth 0 line)))
      (setq full-file (expand-file-name file site-directory))
      (unless (string= toc-rel-file file)
        ;;(setq this-level 0)
        ;;(mapc (lambda(c) (when (eq c ?/) (setq this-level (1+ this-level)))) file)
        (setq dir-title (file-name-nondirectory
                         (substring (file-name-directory full-file) 0 -1)))
        (when (and pending-li
                   (= curr-level this-level))
          (insert "</li>\n")(setq pending-li nil))
        (while (< curr-level this-level)
          (unless (= 1 curr-level)
            (if (not html-toc-no-dir-line)
                (progn
                  (insert "</li>\n")
                  (insert "<li><span onclick=\"html_toc_hs(this)\" class=\"html-toc-hs\">"
                          dir-title "</span>\n"))
              (insert " <span onclick=\"html_toc_hs(this)\" class=\"html-toc-hs\">"
                      "[+]</span></li>\n<li>\n")
              ))
          (insert "<ul>\n")
          (setq curr-level (1+ curr-level))
          )
        (while (> curr-level this-level)
          (setq curr-level (1- curr-level))
          (insert "</li>\n")(setq pending-li nil)
          (insert "</ul>\n</li>\n"))
        (insert (format "<li><a href=\"%s\">%s</a>" file title))
        (setq pending-li t)
        ))
    (while (> curr-level 0)
      (insert "</li>\n")(setq pending-li nil)
      (insert "</ul>\n")
      (unless (= 0 curr-level) (insert "</li>\n"))
      (setq curr-level (1- curr-level))
      ))
  (insert "</li>\n")
  (insert "</ul>\n"))

(defun html-toc-get-title(file)
  (save-excursion
    (with-temp-buffer
      (insert-file-contents file nil 0 1000)
      (goto-char (point-min))
      (when (search-forward-regexp "<title>\\(.*\\)</title>" nil t)
        (match-string 1)))))

(defun html-toc-parse-toc(toc-str)
  (let ((nodes))
    (with-temp-buffer
      (insert toc-str)
      (setq nodes (xml-parse-region (point-min) (point-max))))
    ))

(defun html-toc-get-hrefs(nodes)
  (let ((atags (html-toc-get-atags nodes)))
    (mapcar (lambda(atag)
              (xml-get-attribute atag 'href))
            atags)))
(defun html-toc-get-atags(nodes)
  (let ((atags))
    (dolist (node nodes)
      (when (listp node)
        (setq atags (append atags (xml-get-children node 'a)))
        (setq atags (append atags (html-toc-get-atags (xml-node-children node))))))
    atags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frames and viewing
(defcustom html-toc-frames-default-name "html-toc-frames.html"
  "Default file name sans directory for frames file."
  :type 'string)

(defvar html-toc-frames-contents
    "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>Frames for html-toc</title>
  </head>
  <frameset cols=\"20%, 80%\">
    <frame name=\"html-toc-TOC\" src=\"%%TOCFILE%%\"/>
    <frame name=\"html-toc-Document\" />
    <noframes>
      <body>
        Html frame support required
      </body>
    </noframes>
  </frameset>
</html>
")

(defun html-toc-view-frames-file()
  "View frames file written by `html-toc-write-frames-file'."
  (interactive)
  (html-site-ensure-site-defined)
  (let ((frames-file (html-site-current-frames-file)))
    (unless (< 0 (length frames-file))
      (error "There is no frames file set for site \"%s\"" html-site-current))
    ;;(message "frames-file=%s" frames-file)(sit-for 4)
    (unless (file-exists-p frames-file)
      (html-toc-write-frames-file))
    (browse-url-of-file frames-file)))

;; (defun html-toc-frames-file-name()
;;   "Return name of file written by `html-toc-write-frames-file'."
;;   (html-toc-get-site)
;;   (expand-file-name html-toc-frames-default-name html-move-site-directory))

(defun html-toc-write-frames-file()
  "Write a frames file.
This frames file should load the table of contents build by
`html-toc-write-toc-file' in one frame and shows the documents in
another.

The contents of the frames file is defined by
`html-toc-frames-contents'.

Returns the file name of the written or existing frames file.

You may also want to look at `html-wtoc-write-pages-with-toc'."
  (interactive)
  ;;(html-toc-get-site)
  (html-site-ensure-site-defined)
  (let* ((frames-file (html-site-current-frames-file))
         (frames-cont html-toc-frames-contents)
         (toc-file (html-toc-write-toc-file))
         toc-file-relative)
    (when toc-file
      (setq toc-file-relative (file-relative-name
                               toc-file
                               (file-name-directory frames-file)))
      (save-match-data
        (unless (string-match "%%TOCFILE%%" frames-cont)
          (error "Can't find %%TOCFILE%% in html-toc-frames-contents"))
        (setq frames-cont (replace-match toc-file-relative t t frames-cont)))
      (with-current-buffer (find-file-noselect frames-file)
        (erase-buffer)
        (insert frames-cont)
        (save-buffer))
      frames-file)))

(defconst html-toc-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [html-toc-view-frames-file]
      (list 'menu-item "View Frames File" 'html-toc-view-frames-file))
    (define-key map [html-toc-write-frames-file]
      (list 'menu-item "Write Frames File" 'html-toc-write-frames-file))
    (define-key map [html-toc-write-toc-file]
      (list 'menu-item "Write TOC File for Frames" 'html-toc-write-toc-file))
    (define-key map [html-toc-sep1] (list 'menu-item "--"))
    (define-key map [html-toc-edit-pages-file]
      (list 'menu-item "Edit List of Pages for TOC" 'html-site-edit-pages-file))
    (define-key map [html-toc-create-pages-file]
      (list 'menu-item "Write List of Pages for TOC" 'html-site-create-pages-file))
    map))



(provide 'html-toc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-toc.el ends here
