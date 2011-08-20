;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/google-maps-settings.el
;; Time-stamp: <2010-08-22 16:42:02 Sunday by taoshanwen>

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

(require 'google-maps)

(eal-define-keys
 'google-maps-static-mode-map
 `(("t"   sb-toggle-keep-buffer)
   ("'"   switch-to-other-buffer)
   ("="   google-maps-static-zoom-in)
   ("1"   delete-other-windows)
   ("2"   split-window-vertically)
   ("3"   split-window-horizontally)
   ("o"   other-window)
   ("C-t" google-maps-static-set-maptype)))

(defun google-maps-settings ()
  "Settings for `google-maps'.")

(eval-after-load "google-maps"
  `(google-maps-settings))

(provide 'google-maps-settings)
