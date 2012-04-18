;;; gobject-c-mode.el --- minor mode for editing GObject-based C source code
;; Copyright (C) 2010,2011 Daiki Ueno <ueno@unixuser.org>

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: GObject, C, coding style

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Code:

(autoload 'gobject-c-align-arglist-region "gobject-c-align")
(autoload 'gobject-c-align-func-decls-region "gobject-c-align")
(autoload 'gobject-c-gen-class-name "gobject-c-gen")
(autoload 'gobject-c-gen-class-name-CamelCase "gobject-c-gen")
(autoload 'gobject-c-gen-iface-decl "gobject-c-gen")
(autoload 'gobject-c-gen-class-decl "gobject-c-gen")
(autoload 'gobject-c-gen-property-accessors "gobject-c-gen")

(defvar gobject-c-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-ga" 'gobject-c-align-arglist-region)
    (define-key keymap "\C-c\C-gf" 'gobject-c-align-func-decls-region)
    (define-key keymap "\C-c\C-gc" 'gobject-c-gen-class-name)
    (define-key keymap "\C-c\C-g\C-c" 'gobject-c-gen-class-name-CamelCase)
    (define-key keymap "\C-c\C-gI" 'gobject-c-gen-iface-decl)
    (define-key keymap "\C-c\C-gC" 'gobject-c-gen-class-decl)
    (define-key keymap "\C-c\C-gP" 'gobject-c-gen-property-accessors)
    keymap))

;;;###autoload
(define-minor-mode gobject-c-mode
  "A minor-mode for editing GObject-based C source code."
  nil " GObject" gobject-c-mode-map)

(provide 'gobject-c-mode)

;;; gobject-c-mode.el ends here
