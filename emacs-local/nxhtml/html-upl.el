;;; html-upl.el --- Uploading of web sites

;; Copyright (C) 2006 Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Mon Mar 06 19:09:19 2006
(defconst html-upl:version "0.2") ;; Version:
;; Last-Updated: Wed May 24 06:56:46 2006 (7200 +0200)
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `html-site'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
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
(require 'html-site)

(defcustom html-upl-dir
  (file-name-as-directory
   (expand-file-name
    "html-upl"
    (file-name-directory
     (if load-file-name load-file-name buffer-file-name))))

  "Directory where the tools needed are located.
The tools for html-upl includes:

- ftpsync.pl
"
  :type 'directory
  :group 'html-upl)

;;; Use tramp-tramp-file-p instead:
;; (defun html-upl-file-name-is-local(file-name)
;;   "Return nil unless FILE-NAME is a Tramp file name."
;;   (save-match-data
;;     (not (string-match "^/[a-z]+:" file-name))))

;; (defun html-upl-remote-to-local(remote-file)
;;   (let ((remote-site-dir (html-site-current-web-dir)))
;;     (unless (html-site-dir-contains remote-site-dir remote-file)
;;       (error "")))
;;   )

(defun html-upl-view-remote()
  (interactive)
  (let ((url (html-site-local-to-web buffer-file-name nil)))
    (browse-url url)))
(defun html-upl-view-remote-with-toc()
  (interactive)
  (let ((url (html-site-local-to-web buffer-file-name t)))
    (browse-url url)))
(defun html-upl-view-remote-frames()
  (interactive)
  (let ((url (html-site-local-to-web (html-site-current-frames-file) nil)))
    (browse-url url)))

(defun html-upl-upload-site-with-toc()
  (interactive)
  (html-upl-upload-site1 t))
(defun html-upl-upload-site()
  (interactive)
  (html-upl-upload-site1 nil))
(defun html-upl-upload-site1(with-toc)
  (html-site-ensure-site-defined)
  (let ((local-dir (if with-toc
                       (html-site-current-merge-dir)
                     (html-site-current-site-dir)))
        (ftp-host (html-site-current-ftp-host))
        (ftp-user (html-site-current-ftp-user))
        (ftp-pw (html-site-current-ftp-password))
        (ftp-dir (if with-toc
                     (html-site-current-ftp-wtoc-dir)
                   (html-site-current-ftp-dir)))
        (ftpsync-pl (expand-file-name "ftpsync.pl" html-upl-dir))
        )
    (unless (< 0 (length ftp-host))
      (error "Ftp host not defined"))
    (unless (< 0 (length ftp-user))
      (error "Ftp user not defined"))
    (unless (< 0 (length ftp-dir))
      (if with-toc
          (error "Ftp remote directory for pages with TOC not defined")
        (error "Ftp remote directory not defined")))
    (unless (< 0 (length ftp-pw))
      (setq ftp-pw (html-site-get-ftp-pw)))
    (let* (
           (buffer (noshell-procbuf-setup "subprocess for upload"))
           (remote-url (concat "ftp://" ftp-user ":" ftp-pw "@" ftp-host ftp-dir))
           (opt (list
                 "-v"
                 "-p"
                 local-dir
                 remote-url)))
      (apply 'noshell-procbuf-run
             buffer
             "d:/Perl/bin/perl.exe" "-w"
             ftpsync-pl
             opt
             ))))

(defun html-upl-upload-file(filename)
  (interactive (list
                (let ((f (file-relative-name buffer-file-name)))
                  (read-file-name "File: " nil nil t f))
                ))
  (html-site-ensure-file-in-site filename)
  (let* ((buffer (get-file-buffer filename))
         (remote-file (html-site-local-to-remote filename nil))
         (remote-buffer (get-file-buffer remote-file)))
    (when (or (not (buffer-modified-p buffer))
              (and
               (y-or-n-p (format "Buffer %s is modified. Save buffer and copy? "
                                (buffer-name buffer)))
               (with-current-buffer buffer
                 (save-buffer)
                 (not (buffer-modified-p)))))
      (copy-file filename
                 (html-site-local-to-remote filename nil)
                 0)
      (when remote-buffer
        (with-current-buffer remote-buffer
          (revert-buffer nil t t))))))

(defun html-upl-edit-remote-file()
  (interactive)
  (html-upl-edit-remote-file1 nil))
(defun html-upl-edit-remote-file-with-toc()
  (interactive)
  (html-upl-edit-remote-file1 t))

(defun html-upl-edit-remote-file1(with-toc)
  (html-site-ensure-buffer-in-site)
  (let* ((remote-root (concat "/ftp:"
                              (html-site-current-ftp-user)
                              "@" (html-site-current-ftp-host)
                              ":"
                              (if with-toc
                                  (html-site-current-ftp-wtoc-dir)
                                (html-site-current-ftp-dir))))
         (remote-file (html-site-path-in-mirror (html-site-current-site-dir)
                                                buffer-file-name
                                                remote-root)))
    (find-file remote-file)))

(provide 'html-upl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-upl.el ends here

;; (defun html-site-local-to-remote-path(local-file protocol with-toc)
;;   (let ((remote-dir (if (eq protocol 'ftp)
;;                         (if with-toc
;;                             (html-site-current-ftp-wtoc-dir)
;;                           (html-site-current-ftp-dir))
;;                       (if with-toc
;;                           (html-site-current-web-wtoc-dir)
;;                         (html-site-current-web-dir)))))
;;     (html-site-path-in-mirror
;;      (html-site-current-site-dir) local-file remote-dir)))

;; (defun html-site-local-to-web(local-file with-toc)
;;   (let ((web-file (html-site-local-to-remote-path local-file 'http with-toc))
;;         (web-host (html-site-current-web-host)))
;;     (save-match-data
;;       (unless (string-match "^https?://" web-host)
;;         (setq web-host (concat "http://" web-host))))
;;     (when (string= "/" (substring web-host -1))
;;       (setq web-host (substring web-host 0 -1)))
;;     (concat web-host web-file)
;;     ))

