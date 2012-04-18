;;; gobject-c-gen.el --- GObject C code generation
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

(require 'gobject-c-align)

(defvar gobject-c-package nil)
(defvar gobject-c-class nil)

(defun gobject-c--parse-CamelCase (string)
  (let (case-fold-search
	(index 0)
	result)
    (while (string-match "\\(?:\\`[A-Za-z]\\|[A-Z]\\)[a-z0-9]*" string index)
      (setq result (cons (match-string 0 string) result)
	    index (match-end 0)))
    (nreverse result)))

(defun gobject-c--upcase (words)
  (mapconcat #'upcase words "_"))

(defun gobject-c--downcase (words)
  (mapconcat #'downcase words "_"))

(defun gobject-c--capitalize (words)
  (mapconcat #'capitalize words ""))

(defun gobject-c--read-package-and-class (&optional package-prompt
						    class-prompt)
  (if (or current-prefix-arg
	  (null gobject-c-package))
      (setq gobject-c-package
	    (gobject-c--parse-CamelCase
	     (read-string (or package-prompt
			      "Package (CamelCase): ")))))
  (if (or current-prefix-arg
	  (null gobject-c-class))
      (setq gobject-c-class
	    (gobject-c--parse-CamelCase
	     (read-string (or class-prompt
			      "Class (CamelCase): ")))))
  (list gobject-c-package gobject-c-class))

(defun gobject-c-gen-class-name (&optional arg)
  (interactive)
  "Insert the class name before the current point."
  (gobject-c--read-package-and-class)
  (insert (gobject-c--downcase gobject-c-package) "_"
	  (gobject-c--downcase gobject-c-class)))
  
(defun gobject-c-gen-class-name-CamelCase ()
  (interactive)
  "Insert the class name (in CamelCase) before the current point."
  (gobject-c--read-package-and-class)
  (insert (gobject-c--capitalize gobject-c-package)
	  (gobject-c--capitalize gobject-c-class)))
  
(defun gobject-c-gen-interface-decl (package iface)
  "Insert interface declaration for PACKAGE and IFACE"
  (interactive (gobject-c--read-package-and-class
		nil "Interface (CamelCase): "))
  (insert "\
#define " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase iface) " (" (gobject-c--downcase package) "_" (gobject-c--downcase iface) "_get_type())
#define " (gobject-c--upcase package) "_" (gobject-c--upcase iface) "(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase iface) ", " (gobject-c--capitalize package) (gobject-c--capitalize iface)"))
#define " (gobject-c--upcase package) "_IS_" (gobject-c--upcase iface) "(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase iface) "))
#define " (gobject-c--upcase package) "_" (gobject-c--upcase iface) "_GET_IFACE(obj) (G_TYPE_INSTANCE_GET_INTERFACE ((obj), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase iface) ", " (gobject-c--capitalize package) (gobject-c--capitalize iface) "Iface))
"))

(defun gobject-c-gen-class-decl (package class)
  "Insert class declaration for PACKAGE and CLASS."
  (interactive (gobject-c--read-package-and-class))
  (insert "\
#define " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase class) " (" (gobject-c--downcase package) "_" (gobject-c--downcase class) "_get_type())
#define " (gobject-c--upcase package) "_" (gobject-c--upcase class) "(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase class) ", " (gobject-c--capitalize package) (gobject-c--capitalize class)"))
#define " (gobject-c--upcase package) "_" (gobject-c--upcase class) "_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase class) ", " (gobject-c--capitalize package) (gobject-c--capitalize class) "Class))
#define " (gobject-c--upcase package) "_IS_" (gobject-c--upcase class) "(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase class) "))
#define " (gobject-c--upcase package) "_IS_" (gobject-c--upcase class) "_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase class) "))
#define " (gobject-c--upcase package) "_" (gobject-c--upcase class) "_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase class) ", " (gobject-c--capitalize package) (gobject-c--capitalize class) "Class))
"))

(defun gobject-c-gen-property-accessors (package class)
  "Insert property accessors for PACKAGE and CLASS."
  (interactive (gobject-c--read-package-and-class))
  (let (arglist-start)
    (insert "\
static void
" (gobject-c--downcase package) "_" (gobject-c--downcase class) "_set_property (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
guint prop_id,
const GValue *value, 
GParamSpec *pspec")
    (gobject-c-align-arglist-region arglist-start (point))
    (insert ")
{
    switch (prop_id) 
        {
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
            break;
        }
}

static void
" (gobject-c--downcase package) "_" (gobject-c--downcase class) "_get_property (")
    (setq arglist-start (point-marker))
    (insert "GObject *object, 
guint prop_id,
GValue *value, 
GParamSpec *pspec")
    (gobject-c-align-arglist-region arglist-start (point))
    (insert ")
{
    switch (prop_id) 
        {
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
            break;
        }
}
")))

(defun gobject-c-gen-private-struct (package class)
  "Insert private class structure for PACKAGE and CLASS."
  (interactive (gobject-c--read-package-and-class))
  (let ((point (point)))
    (insert "\
#define " (gobject-c--upcase package) "_" (gobject-c--upcase class) "_GET_PRIVATE(obj) \
    (G_TYPE_INSTANCE_GET_PRIVATE ((obj), " (gobject-c--upcase package) "_TYPE_" (gobject-c--upcase class) ", " (gobject-c--capitalize package) (gobject-c--capitalize class) "Private))")
    (c-backslash-region point (point) nil))
  (insert "\n\n\
struct _" (gobject-c--capitalize package) (gobject-c--capitalize class) "Private
{
};
"))

(provide 'gobject-c-gen)

;;; gobject-c-gen.el ends here
