;;; perl-skeletons.el --- Skeletons to edit perl a bit easier

;; Copyright (C) 2012  Michael S. Joyce

;; Author: Michael S. Joyce <michael@negativespace.net>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(global-set-key [f5] 'perl-pod-skel)
(define-skeleton perl-pod-skel
  "Insert a POD section for a Perl method."
  nil
  "" \n
  "=item B<< $obj->" (setq methodname (skeleton-read "Method name: ")) "() >>" \n
  "" \n
  _ \n
  "" \n
  "=cut" \n
  )

(provide 'perl-skeletons)
;;; perl-skeletons.el ends here

