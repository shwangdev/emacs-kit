;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; Keywords: 
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/svn-misc.el
;; Time-stamp: <2010-09-04 21:08:19 Saturday by taoshanwen>

;; This  file is free  software; you  can redistribute  it and/or
;; modify it under the terms of the GNU General Public License as
;; published by  the Free Software Foundation;  either version 3,
;; or (at your option) any later version.

;; This file is  distributed in the hope that  it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You  should have  received a  copy of  the GNU  General Public
;; License along with  GNU Emacs; see the file  COPYING.  If not,
;; write  to  the Free  Software  Foundation,  Inc., 51  Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Installation:
;;
;; Copy svn-misc.el to your load-path and add to your .emacs:
;;
;; (require 'svn-misc)

;;; History:
;;
;; 2010-4-1
;;      * initial version 1.0.

;;; Code:

;;;###autoload
(defalias 'svn 'svn-status)

;;;###autoload
(defun svn-status-this-dir-hide (&optional arg)
  "运行`svn-status-this-directory'后隐藏unmodified和unknown"
  (interactive "P")
  (let* ((file buffer-file-name))
    (svn-status-hide default-directory arg)
    (if (and (vc-registered file) (not (vc-up-to-date-p file)))
        (svn-status-goto-file-name (file-name-nondirectory file)))))

;;;###autoload
(defun svn-status-goto-first-line ()
  "Goto first line."
  (interactive)
  (svn-status-goto-root)
  (svn-status-next-line 1)
  (unless (svn-status-get-line-information)
    (svn-status-previous-line 1)))

;;;###autoload
(defun svn-status-goto-root ()
    "Goto root (\".\")."
    (interactive)
    (setq svn-status-root-return-info (svn-status-get-line-information))
    (svn-status-goto-file-name "."))

;;;###autoload
(defun svn-resolve-conflicts-current-file ()
  (interactive)
  (svn-resolve-conflicts buffer-file-name))

;;;###autoload
(defun svn-resolved-current-file ()
  (interactive)
  (svn-status-resolved buffer-file-name))

;;;###autoload
(defun svn-status-my-emacs-dir ()
  "Run `svn-status' in `my-emacs-path'."
  (interactive)
  (svn-status my-emacs-path))

;;;###autoload
(defun visit-svn-status ()
  "访问*svn-status* buffer"
  (interactive)
  (let ((buffer (get-buffer svn-status-buffer-name)))
    (if buffer (switch-to-buffer buffer) (call-interactively 'svn-status-hide))))

(provide 'svn-misc)

;;; svn-misc.el ends here
