;;; gobject-c-align.el --- GObject C code alignment
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

(require 'cc-mode)
(require 'regexp-opt)

(defvar gobject-c-whitespace
  " \f\t\n\r\v")

(defvar gobject-c-max-line-width 80)

(defun gobject-c--make-arg (type-start type-end arg-name-start arg-name-end)
  (vector type-start type-end arg-name-start arg-name-end))

(defun gobject-c--arg-type-start (arg)
  (aref arg 0))

(defun gobject-c--arg-arg-name-start (arg)
  (aref arg 2))

(defun gobject-c--arg-type-width (arg)
  (- (gobject-c--arg-arg-name-start arg)
     (gobject-c--arg-type-start arg)))

(defun gobject-c--arglist-type-column-width (arglist)
  (let ((width 0)
	length)
    (while arglist
      (setq length (gobject-c--arg-type-width (car arglist)))
      (if (> length width)
	  (setq width length))
      (setq arglist (cdr arglist)))
    width))

(defun gobject-c--arglist-arg-name-column-width (arglist)
  (let ((width 0)
	length)
    (while arglist
      (setq length (- (aref (car arglist) 3) ;arg-name-end
		      (gobject-c--arg-arg-name-start (car arglist))))
      (if (> length width)
	  (setq width length))
      (setq arglist (cdr arglist)))
    width))

(defun gobject-c--parse-arglist (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let (type-start
	    type-end
	    arg-name-start
	    arg-name-end
	    arg
	    arglist
	    point)
	(goto-char (point-min))
	(while (and (not (eobp))
		    (setq type-start (point-marker))
		    (if (prog1 (re-search-forward
				(concat
				 "["
				 (regexp-quote gobject-c-whitespace)
				 "]*,["
				 (regexp-quote gobject-c-whitespace)
				 "]*")
				nil 'noerror)
			  (setq point (point)))
			(goto-char (match-beginning 0))
		      (goto-char (point-max))))
	  (setq arg-name-end (point-marker))
	  (c-backward-token-1)
	  (setq arg-name-start (point-marker))	  
	  (skip-chars-backward (concat gobject-c-whitespace "*"))
	  (setq type-end (point-marker))
	  (setq arg (gobject-c--make-arg type-start type-end
					 arg-name-start arg-name-end)
		arglist (cons arg arglist))
	  (goto-char point))
	arglist))))

(defun gobject-c--make-func-decl (type-start type-end
				  func-name-start func-name-end
				  arglist-start arglist-end
				  func-decl-end
				  arglist)
  (vector type-start type-end func-name-start func-name-end
	  arglist-start arglist-end func-decl-end arglist))

(defun gobject-c--func-decl-start (func-decl)
  (aref func-decl 0))

(defun gobject-c--func-decl-func-name-start (func-decl)
  (aref func-decl 2))

(defun gobject-c--func-decl-func-name-end (func-decl)
  (aref func-decl 3))

(defun gobject-c--func-decl-arglist-start (func-decl)
  (aref func-decl 4))

(defun gobject-c--func-decl-arglist-end (func-decl)
  (aref func-decl 5))

(defun gobject-c--func-decl-end (func-decl)
  (aref func-decl 6))

(defun gobject-c--func-decl-arglist (func-decl)
  (aref func-decl 7))

(defun gobject-c--func-decl-type-width (func-decl)
  (- (gobject-c--func-decl-func-name-start func-decl)
     (gobject-c--func-decl-start func-decl)))

(defun gobject-c--func-decl-func-name-width (func-decl)
  (- (gobject-c--func-decl-arglist-start func-decl)
     (gobject-c--func-decl-func-name-start func-decl)))

(defun gobject-c--func-decls-type-column-width (func-decls)
  (let ((width 0)
	length)
    (while func-decls
      (setq length (gobject-c--func-decl-type-width (car func-decls)))
      (if (> length width)
	  (setq width length))
      (setq func-decls (cdr func-decls)))
    width))

(defun gobject-c--func-decls-func-name-column-width (func-decls
						     start-column
						     arglist-column-width)
  (let ((width 0)
	length)
    (while func-decls
      (setq length (gobject-c--func-decl-func-name-width (car func-decls)))
      
      (if (and (<= (+ start-column 
		      length
		      arglist-column-width)
		   gobject-c-max-line-width)
	       (> length width))
	  (setq width length))
      (setq func-decls (cdr func-decls)))
    width))

(defun gobject-c--func-decls-arglist-type-column-width (func-decls)
  (let ((width 0)
	arglist-type-column-width)
    (while func-decls
      (setq arglist-type-column-width
	    (gobject-c--arglist-type-column-width
	     (gobject-c--func-decl-arglist (car func-decls))))
      (if (> arglist-type-column-width width)
	  (setq width arglist-type-column-width))
      (setq func-decls (cdr func-decls)))
    width))

(defun gobject-c--func-decls-arglist-arg-name-column-width (func-decls)
  (let ((width 0)
	arglist-arg-name-column-width)
    (while func-decls
      (setq arglist-arg-name-column-width
	    (gobject-c--arglist-arg-name-column-width
	     (gobject-c--func-decl-arglist (car func-decls))))
      (if (> arglist-arg-name-column-width width)
	  (setq width arglist-arg-name-column-width))
      (setq func-decls (cdr func-decls)))
    width))

(defun gobject-c--parse-func-decl (beg end)
  ;; Parse one func-decl in BEG END.  BEG and END must point to the
  ;; beginning/end of the func-decl.
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-max))
      (let (arglist-start
	    arglist-end
	    func-name-start
	    func-name-end)
	;; foo *bar (baz a) G_GNUC_CONST;
	;;                              ^
	(unless (looking-back (concat "["
				      (regexp-quote gobject-c-whitespace)
				      "]*\\(?:[A-Z_]*\\>["
				      (regexp-quote gobject-c-whitespace)
				      "]*\\)*["
				      (regexp-quote gobject-c-whitespace)
				      "]*;["
				      (regexp-quote gobject-c-whitespace)
				      "]*")
			    nil
			    t)
	  (error "No func-decl at point"))
	(goto-char (match-beginning 0))
	;; foo *bar (baz a) G_GNUC_CONST;
	;;                ^
	(unless (eq (char-before) ?\))
	  (error "No arglist at point"))
	(setq arglist-end (point-marker))
	(c-backward-sexp)		;skip arglist
	;; foo *bar (baz a) G_GNUC_CONST;
	;;          ^
	(setq arglist-start (point-marker))
	(skip-chars-backward gobject-c-whitespace)
	;; foo *bar (baz a) G_GNUC_CONST;
	;;        ^
	(setq func-name-end (point-marker))
	;;(c-backward-token-2)
	(c-backward-sexp)		;may be either an identifier
					;or a pointer
	;; foo *bar (baz a) G_GNUC_CONST;
	;;      ^
	(setq func-name-start (point-marker))
	(skip-chars-backward (concat gobject-c-whitespace "*"))
	;; foo *bar (baz a) G_GNUC_CONST;
	;;   ^
	(gobject-c--make-func-decl (point-min-marker) (point-marker)
				   func-name-start func-name-end
				   arglist-start arglist-end
				   (point-max-marker)
				   (gobject-c--parse-arglist
				    (1+ arglist-start)
				    (1- arglist-end)))))))

(defun gobject-c--normalize-arglist (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward (concat "[" 
					(regexp-quote gobject-c-whitespace)
					"]+")
				nil t)
	(replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward " *, *" nil t)
	(replace-match ",\n")))))

(defun gobject-c--normalize-func-decl (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward (concat "["
					(regexp-quote gobject-c-whitespace)
					"]+")
				nil t)
	(replace-match " ")))))

(defun gobject-c--indent-identifier-to-column (column)
  (when (looking-back "\*+" nil t)
    (setq column (- column (- (match-end 0) (match-beginning 0))))
    (goto-char (match-beginning 0)))
  (let (indent-tabs-mode)
    (indent-to-column column)))

(defun gobject-c--expand-region-to-arglist-extent (beg end)
  (setq beg (save-excursion
	      (goto-char beg)
	      (c-beginning-of-decl-1)
	      (point))
	end (save-excursion
	      (goto-char end)
	      (c-end-of-decl-1)
	      (point)))
  (unless (and (eq (char-before beg) ?\()
	       (eq (char-after end) ?\)))
    (error "No arglist around point"))
  (list beg end))

(defun gobject-c-align-arglist-region (beg end &optional type-column-width)
  "Reformat argument list in the region between BEG and END.
It applies proper alignment rule.

Example:

 void eek_key_set_labels (EekKey *self, const gchar **labels, gint num_groups, gint num_levels);

...will be...

 void eek_key_set_labels (EekKey       *self,
                          const gchar **labels,
                          gint          num_groups,
                          gint          num_levels);

When called interactively and no region is active, it looks
around the point and tries to find the extent containing argument list."
  (interactive (apply #'gobject-c--expand-region-to-arglist-extent
		      (if (region-active-p)
			  (list (region-beginning) (region-end))
			(list (point) (point)))))
  (save-excursion
    (let ((indent-level (progn (goto-char beg) (current-column)))
	  arg
	  arglist
	  column)
      (save-restriction
	(narrow-to-region beg end)
	(setq arglist (gobject-c--parse-arglist (point-min) (point-max)))
	;; This may move markers in arglist.
	(gobject-c--normalize-arglist (point-min) (point-max))
	(unless type-column-width
	  (setq type-column-width (gobject-c--arglist-type-column-width
				   arglist)))
	(while arglist
	  (setq arg (car arglist))
	  (goto-char (gobject-c--arg-type-start arg))
	  (if (bobp)
	      (setq column 0)
	    (setq column indent-level))
	  (gobject-c--indent-identifier-to-column column)
	  ;; Don't indent for no-arg-name arg.
	  (unless (= (gobject-c--arg-type-start arg)
		     (gobject-c--arg-arg-name-start arg))
	    (setq column (+ column type-column-width))
	    (goto-char (gobject-c--arg-arg-name-start arg))
	    (gobject-c--indent-identifier-to-column column))
	  (setq arglist (cdr arglist)))))))

(defun gobject-c-align-func-decls-region (beg end)
  "Reformat function declarations in the region between BEG and END.
It applies proper alignment rule.

Example:

 void eek_key_set_labels (EekKey *self, const gchar **labels, gint num_groups, gint num_levels);
 G_CONST_RETURN gchar *eek_key_get_label (EekKey *self);
 void eek_key_set_group_level (EekKey *self, gint group, gint level);
 gint eek_key_get_group (EekKey *self);
 void eek_key_long_method_name_which_requires_line_wrap (void);
 void eek_key_even_longer_method_name_which_occupies_preceding_spaces (void);

...will be...

 void                  eek_key_set_labels      (EekKey       *self,
                                                const gchar **labels,
                                                gint          num_groups,
                                                gint          num_levels);
 G_CONST_RETURN gchar *eek_key_get_label       (EekKey       *self);
 void                  eek_key_set_group_level (EekKey       *self,
                                                gint          group,
                                                gint          level);
 gint                  eek_key_get_group       (EekKey       *self);
 void                  eek_key_long_method_name_which_requires_line_wrap
                                               (void);
 void eek_key_even_longer_method_name_which_occupies_preceding_spaces
                                               (void);"
  (interactive "r")
  (save-excursion
    (let ((indent-level (save-excursion
			  (goto-char beg)
			  (skip-chars-forward gobject-c-whitespace)
			  (current-column)))
	  func-decl-end
	  func-decl
	  func-decls
	  pointer
	  func-name-width
	  type-column-width
	  func-name-column-width
	  arglist-type-column-width
	  arglist-arg-name-column-width
	  arglist-column-width
	  column)
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (search-forward ";" nil t)
	  ;; XXX: Should skip non-func-decl statements.
	  (setq func-decl-end (point-marker))
	  (c-beginning-of-statement-1)
	  (setq func-decl (gobject-c--parse-func-decl (point-marker)
						      func-decl-end)
		func-decls (cons func-decl func-decls))
	  (goto-char func-decl-end))
	;; This may move markers in func-decls.
	(setq pointer func-decls)
	(while pointer
	  (setq func-decl (car pointer))
	  (gobject-c--normalize-func-decl (gobject-c--func-decl-start func-decl)
					  (gobject-c--func-decl-end func-decl))
	  (setq pointer (cdr pointer)))
	(setq type-column-width
	      (gobject-c--func-decls-type-column-width func-decls)
	      arglist-type-column-width
	      (gobject-c--func-decls-arglist-type-column-width func-decls)
	      arglist-arg-name-column-width
	      (gobject-c--func-decls-arglist-arg-name-column-width func-decls)
	      arglist-column-width
	      (+ arglist-type-column-width
		  arglist-arg-name-column-width
		  3)			;(length "();")
	      func-name-column-width
	      (gobject-c--func-decls-func-name-column-width
	       func-decls
	       (+ indent-level
		  type-column-width)
	       arglist-column-width))
	(setq pointer func-decls)
	(while pointer
	  (setq func-decl (car pointer))
	  (goto-char (gobject-c--func-decl-start func-decl))
	  (setq column indent-level)
	  (gobject-c--indent-identifier-to-column column)
	  ;; Align type column.
	  (setq func-name-width
		(- (gobject-c--func-decl-func-name-end func-decl)
		   (gobject-c--func-decl-func-name-start func-decl)))
	  (if (> (+ column type-column-width func-name-width)
		 gobject-c-max-line-width)
	      (setq column
		    (+ column
		       (gobject-c--func-decl-type-width func-decl)))
	    (setq column (+ column type-column-width)))
	  (goto-char (gobject-c--func-decl-func-name-start func-decl))
	  (gobject-c--indent-identifier-to-column column)
	  ;; Align func-name column.
	  (when (> (- (gobject-c--func-decl-func-name-end func-decl)
		      (point))
		   func-name-column-width)
	    (goto-char (gobject-c--func-decl-func-name-end func-decl))
	    (insert "\n")
	    (setq column (+ indent-level type-column-width)))
	  (setq column (+ column func-name-column-width))
	  (goto-char (gobject-c--func-decl-arglist-start func-decl))
	  (gobject-c--indent-identifier-to-column column)
	  ;; Align arglist.
	  (gobject-c-align-arglist-region (1+ (point-marker))
					  (1- (gobject-c--func-decl-arglist-end
					       func-decl))
					  arglist-type-column-width)
	  (setq pointer (cdr pointer)))))))

(provide 'gobject-c-align)

;;; gobject-c-align.el ends here
